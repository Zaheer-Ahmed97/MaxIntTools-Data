# ============================================================================
# Commodity Prices and Industrial Production in Pakistan (Jul 2016 - Nov 2025)
# PART 1: DATA LOADING, TRANSFORMATIONS, UNIT ROOT TESTING
# ============================================================================
path   <- "C:/Users/ahmed/Desktop/IfADo Projects/"   # <-- EDIT if your files are in a different folder
outputs_dir <- file.path(path, "outputs")
plots_dir   <- file.path(path, "plots")
dir.create(outputs_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir,   showWarnings = FALSE, recursive = TRUE)

suppressMessages({
  library(readxl); library(dplyr); library(tidyr); library(lubridate)
  library(zoo);     library(tseries); library(urca);    library(car)
  library(lmtest);  library(strucchange)
  library(ARDL);    library(randomForest); library(gbm)
})

# ============================================================
# STEP 1: PATHS + ALL HELPER FUNCTIONS
# ============================================================
rmse_fn <- function(a,p) sqrt(mean((a-p)^2, na.rm=TRUE))
mae_fn  <- function(a,p) mean(abs(a-p), na.rm=TRUE)
mape_fn <- function(a,p) mean(abs((a-p)/a), na.rm=TRUE)*100

parse_mon_yr <- function(x) {
  mon_map <- c(Jan="01",Feb="02",Mar="03",Apr="04",May="05",Jun="06",
               Jul="07",Aug="08",Sep="09",Oct="10",Nov="11",Dec="12")
  x <- trimws(as.character(x))
  out <- as.Date(rep(NA,length(x)))
  for (i in seq_along(x)) {
    if (is.na(x[i])||nchar(x[i])==0) next
    parts <- strsplit(x[i],"-")[[1]]
    m <- mon_map[parts[1]]; if(is.na(m)) next
    out[i] <- as.Date(paste0("20",parts[2],"-",m,"-01"))
  }
  out
}

create_features <- function(dat, lags=6) {
  out <- dat
  for (v in c("QIM","FUEL","ENERGY","WHEAT","GAS"))
    for (k in 1:lags)
      out[[paste0(v,"_lag",k)]] <- dplyr::lag(dat[[v]],k)
  out$month <- as.numeric(format(dat$date,"%m"))
  out$trend <- seq_len(nrow(dat))
  out
}

build_var_X <- function(Y,p) {
  n <- nrow(Y); rows <- (p+1):n; Y_dep <- Y[rows,]
  X <- do.call(cbind, lapply(1:p, function(l) Y[(rows-l),]))
  colnames(X) <- as.vector(outer(colnames(Y),paste0(".l",1:p),paste0))
  list(Y=Y_dep, X=cbind(const=1,X), rows=rows)
}

fit_gbm_corrector <- function(resid_train, X_tr, X_te,
                              n.trees=300, depth=2,
                              shrinkage=0.05, seed=42) {
  r <- resid_train[!is.na(resid_train)]
  yr <- tail(r, nrow(X_tr))
  df <- as.data.frame(cbind(yr=yr, X_tr))
  set.seed(seed)
  fit <- gbm(yr~., data=df, distribution="gaussian",
             n.trees=n.trees, interaction.depth=depth,
             shrinkage=shrinkage, bag.fraction=0.80,
             cv.folds=5, verbose=FALSE)
  best <- gbm.perf(fit, method="cv", plot.it=FALSE)
  pred <- predict(fit, as.data.frame(X_te), n.trees=best)
  list(fit=fit, best=best, pred=pred)
}

fit_rf_corrector <- function(resid_train, X_tr, X_te, seed=42) {
  r  <- resid_train[!is.na(resid_train)]
  yr <- tail(r, nrow(X_tr))
  set.seed(seed)
  fit  <- randomForest(x=X_tr, y=yr, ntree=500,
                       mtry=floor(sqrt(ncol(X_tr))), nodesize=3)
  pred <- predict(fit, X_te)
  list(fit=fit, pred=pred)
}

theme_pub <- function(base_size=12) {
  theme_bw(base_size=base_size) +
    theme(panel.background=element_rect(fill="white"),
          panel.grid.major=element_line(color="grey85",linewidth=0.3),
          panel.grid.minor=element_blank(),
          axis.text=element_text(size=10,color="black"),
          axis.title=element_text(size=11,face="bold"),
          plot.title=element_text(size=13,face="bold",hjust=0),
          plot.subtitle=element_text(size=10,color="grey40"),
          legend.background=element_rect(fill="white",color="grey70"),
          legend.key=element_rect(fill="white"),
          strip.background=element_rect(fill="grey90"),
          strip.text=element_text(face="bold",size=10))
}
theme_publication <- theme_pub

save_tiff <- function(p, fn, width=12, height=8, dpi=600) {
  fp <- file.path(plots_dir, paste0(fn,".tiff"))
  tiff(fp,width=width,height=height,units="in",res=dpi,compression="lzw")
  if(inherits(p,"ggplot")) print(p) else p
  dev.off(); cat("Saved:",fp,"\n")
}

# ============================================================
# STEP 2: DATA LOADING (matches VERIFIED real file structure)
# ============================================================
cat("\n=== STEP 2: DATA LOADING ===\n")

# ---- Industrial Production (QIM) ----
# Header rows 1-5, data starts row 6: col A = date, col B = QIM
ip_raw <- read_excel(file.path(path, "Industrial Production Data.xlsx"),
                     sheet = "Trend Sheet (2)", skip = 5, col_names = FALSE)
ip_data <- data.frame(
  date = as.Date(format(as.Date(ip_raw[[1]]), "%Y-%m-01")),
  QIM  = as.numeric(ip_raw[[2]])
)
ip_data <- ip_data[!is.na(ip_data$date) & !is.na(ip_data$QIM), ]
ip_data <- ip_data[order(ip_data$date), ]
ip_data <- ip_data[!duplicated(ip_data$date), ]
cat("IP data:", nrow(ip_data), "obs |", as.character(min(ip_data$date)),
    "to", as.character(max(ip_data$date)), "\n")

# ---- Wheat Price Data ----
# Right block (base year 2015-16, matches IP's base year): col E=Month,
# col F=Wheat(10kg). Header row 8, data starts row 9.
wheat_raw <- read_excel(file.path(path, "Wheat data.xlsx"),
                        sheet = "Sheet1", skip = 8, col_names = FALSE)
parse_mon_yr <- function(x) {
  mon_map <- c(Jan="01",Feb="02",Mar="03",Apr="04",May="05",Jun="06",
               Jul="07",Aug="08",Sep="09",Oct="10",Nov="11",Dec="12")
  x <- trimws(as.character(x))
  out <- as.Date(rep(NA, length(x)))
  for (i in seq_along(x)) {
    if (is.na(x[i]) || nchar(x[i]) == 0) next
    parts <- strsplit(x[i], "-")[[1]]
    if (length(parts) != 2) next
    mon <- mon_map[substr(parts[1], 1, 3)]
    if (is.na(mon)) next
    out[i] <- as.Date(paste0("20", parts[2], "-", mon, "-01"))
  }
  out
}
wheat_data <- data.frame(
  date  = parse_mon_yr(wheat_raw[[5]]),
  WHEAT = as.numeric(wheat_raw[[6]])
)
wheat_data <- wheat_data[!is.na(wheat_data$date) & !is.na(wheat_data$WHEAT), ]
wheat_data <- wheat_data[order(wheat_data$date), ]
wheat_data <- wheat_data[!duplicated(wheat_data$date), ]
cat("Wheat data:", nrow(wheat_data), "obs |", as.character(min(wheat_data$date)),
    "to", as.character(max(wheat_data$date)), "\n")

# ---- Energy Data ----
# Clean header row 1: Months, Electricity, Gas_MMBTU, Petrol, Diesel, CNG
energy_raw <- read_excel(file.path(path, "Energy data.xlsx"),
                         sheet = "Base Year 2007-08", skip = 1, col_names = FALSE)
colnames(energy_raw) <- c("Month","Electricity","Gas_MMBTU","Petrol","Diesel","CNG")
energy_data <- data.frame(
  date        = parse_mon_yr(energy_raw$Month),
  Electricity = as.numeric(energy_raw$Electricity),
  Gas_MMBTU   = as.numeric(energy_raw$Gas_MMBTU),
  Petrol      = as.numeric(energy_raw$Petrol),
  Diesel      = as.numeric(energy_raw$Diesel)
)
energy_data <- energy_data[!is.na(energy_data$date), ]
energy_data <- energy_data[order(energy_data$date), ]
energy_data <- energy_data[!duplicated(energy_data$date), ]
cat("Energy data:", nrow(energy_data), "obs |", as.character(min(energy_data$date)),
    "to", as.character(max(energy_data$date)), "\n")

# ============================================================
# STEP 3: MERGING OF DATA in Step 2
# ============================================================
cat("\n=== STEP 3: MERGING ===\n")

merged <- ip_data %>%
  inner_join(wheat_data, by = "date") %>%
  inner_join(energy_data[, c("date","Petrol","Diesel","Gas_MMBTU","Electricity")], by = "date")

merged$FUEL   <- (merged$Petrol + merged$Diesel) / 2
merged$ENERGY <- merged$Electricity
merged$GAS    <- merged$Gas_MMBTU
merged <- merged[order(merged$date), ]

cat("Merged dataset:", nrow(merged), "obs |", as.character(min(merged$date)),
    "to", as.character(max(merged$date)), "\n")
cat("Missing values:\n")
print(colSums(is.na(merged[, c("QIM","WHEAT","FUEL","ENERGY","GAS")])))

for (v in c("QIM","WHEAT","FUEL","ENERGY","GAS")) {
  merged[[v]] <- as.numeric(na.approx(merged[[v]], na.rm = FALSE))
}

# Data characteristic worth flagging: GAS is administratively set
cat("\nUnique value counts (flags administered/step-function pricing):\n")
for (v in c("QIM","WHEAT","FUEL","ENERGY","GAS")) {
  cat(" ", v, ":", length(unique(merged[[v]])), "unique of", nrow(merged), "\n")
}

write.csv(merged, file.path(outputs_dir, "clean_merged_data.csv"), row.names = FALSE)
cat("\nSaved: clean_merged_data.csv\n")


# ============================================================
# STEP 4: BUILD THREE TRANSFORMATIONS (Level, Log, Box-Cox)
# ============================================================
cat("\n=== STEP 4: TRANSFORMATIONS ===\n")

VARS <- c("QIM","WHEAT","FUEL","ENERGY","GAS")

boxcox_lambdas <- list()
merged_log <- merged
merged_bc  <- merged

for (v in VARS) {
  merged_log[[v]] <- log(merged[[v]])
  
  pt <- car::powerTransform(merged[[v]] ~ 1)
  lambda_v <- unname(pt$lambda)
  boxcox_lambdas[[v]] <- lambda_v
  merged_bc[[v]] <- car::bcPower(merged[[v]], lambda_v)
}

cat("Box-Cox lambdas:\n")
for (v in VARS) cat(" ", v, ":", round(boxcox_lambdas[[v]], 4), "\n")

saveRDS(boxcox_lambdas, file.path(outputs_dir, "boxcox_lambdas.rds"))
write.csv(merged_log, file.path(outputs_dir, "merged_log.csv"), row.names = FALSE)
write.csv(merged_bc,  file.path(outputs_dir, "merged_boxcox.csv"), row.names = FALSE)

# ============================================================
# STEP 5: DESCRIPTIVE STATISTICS at Level
# ============================================================
cat("\n=== STEP 5: DESCRIPTIVE STATISTICS ===\n")
vars_d <- c("QIM","WHEAT","FUEL","ENERGY","GAS")
vars_l <- c("Industrial Production","Wheat","Fuel","Electricity","Gas")
desc_stats <- data.frame(
  Variable=vars_l,
  N     =sapply(vars_d,function(v) sum(!is.na(merged[[v]]))),
  Mean  =sapply(vars_d,function(v) mean(merged[[v]],na.rm=TRUE)),
  SD    =sapply(vars_d,function(v) sd(merged[[v]],na.rm=TRUE)),
  Min   =sapply(vars_d,function(v) min(merged[[v]],na.rm=TRUE)),
  Median=sapply(vars_d,function(v) median(merged[[v]],na.rm=TRUE)),
  Max   =sapply(vars_d,function(v) max(merged[[v]],na.rm=TRUE)),
  CV_pct=sapply(vars_d,function(v){x<-merged[[v]];sd(x,na.rm=T)/mean(x,na.rm=T)*100}),
  Skew  =sapply(vars_d,function(v){x<-merged[[v]][!is.na(merged[[v]])];mean((x-mean(x))^3)/sd(x)^3}),
  Kurt  =sapply(vars_d,function(v){x<-merged[[v]][!is.na(merged[[v]])];mean((x-mean(x))^4)/sd(x)^4-3})
)
print(round(desc_stats[,-1],3))
write.csv(desc_stats,file.path(outputs_dir,"descriptive_statistics.csv"),row.names=FALSE)


# ============================================================
# STEP 6: Times Series Plots: FIGURES 1 & 2
# ============================================================
cat("\n=== FIGURES 1 & 2 ===\n")
ts_long <- merged %>%
  dplyr::select(date,QIM,WHEAT,FUEL,ENERGY,GAS) %>%
  tidyr::pivot_longer(-date,names_to="Variable",values_to="Value") %>%
  mutate(Variable=factor(Variable,levels=c("QIM","WHEAT","FUEL","ENERGY","GAS"),
                         labels=c("Industrial Production (QIM)","Wheat (PKR/10kg)",
                                  "Fuel (PKR/L)","Electricity (PKR/kWh)","Gas (PKR/MMBTU)")))
p1 <- ggplot(ts_long[!is.na(ts_long$Value),],aes(x=date,y=Value,color=Variable))+
  geom_line(linewidth=0.8)+facet_wrap(~Variable,scales="free_y",ncol=1)+
  scale_x_date(date_breaks="1 year",date_labels="%Y")+
  scale_color_manual(values=c("#E63946","#2A9D8F","#E9C46A","#264653","#A8DADC"))+
  labs(title="Time Series: Industrial Production and Commodity Prices",
       subtitle="Monthly 2016-2025 | Source: PBS, OGRA",x="Date",y="Value")+
  theme_pub(12)+theme(legend.position="none")
save_tiff(p1,"Fig1_TimeSeries",width=12,height=14)

ts_log_long <- merged %>%
  dplyr::select(date,QIM,FUEL,ENERGY,WHEAT,GAS) %>%
  tidyr::pivot_longer(-date,names_to="Variable",values_to="Value") %>%
  mutate(Variable=factor(Variable,levels=c("QIM","WHEAT","FUEL","ENERGY","GAS"),
                         labels=c("QIM","Wheat","Fuel","Electricity","Gas")))
p2 <- ggplot(ts_log_long[!is.na(ts_log_long$Value),],aes(x=date,y=Value,color=Variable))+
  geom_line(linewidth=0.9)+
  geom_smooth(method="loess",span=0.3,se=FALSE,linetype="dashed",linewidth=0.5,color="grey30")+
  facet_wrap(~Variable,scales="free_y",ncol=1)+
  scale_x_date(date_breaks="1 year",date_labels="%Y")+
  scale_color_manual(values=c("#E63946","#2A9D8F","#E9C46A","#264653","#F4A261"))+
  labs(title="Raw Series with LOESS Trends",x="Date",y="Value")+
  theme_pub(12)+theme(legend.position="none")
save_tiff(p2,"Fig2_LogTimeSeries",width=12,height=14)


# =================================================================
# STEP 7: UNIT ROOT TESTS (ADF + KPSS) -- ALL THREE TRANSFORMATIONS
# =================================================================
cat("\n=== STEP 7: UNIT ROOT TESTS (ADF + KPSS) ===\n")
run_unitroot_battery <- function(df, label) {
  rows <- lapply(VARS, function(v) {
    x  <- df[[v]]
    dx <- diff(x)
    
    adf_lv <- ur.df(x,  type = "trend", selectlags = "AIC")
    adf_d1 <- ur.df(dx, type = "drift", selectlags = "AIC")
    kpss_lv <- ur.kpss(x,  type = "tau", lags = "short")
    kpss_d1 <- ur.kpss(dx, type = "mu",  lags = "short")
    
    adf_reject_lv   <- adf_lv@teststat[1] < adf_lv@cval[1,2]
    kpss_reject_lv  <- kpss_lv@teststat > kpss_lv@cval[2]
    adf_reject_d1   <- adf_d1@teststat[1] < adf_d1@cval[1,2]
    kpss_reject_d1  <- kpss_d1@teststat > kpss_d1@cval[2]
    
    classification <- if (adf_reject_lv && !kpss_reject_lv) {
      "I(0) - both agree"
    } else if (!adf_reject_lv && kpss_reject_lv && adf_reject_d1 && !kpss_reject_d1) {
      "I(1) - both agree"
    } else {
      "AMBIGUOUS - tests disagree"
    }
    
    data.frame(
      Variable = v,
      ADF_Level_tau  = round(adf_lv@teststat[1], 3),
      ADF_Level_cv5  = round(adf_lv@cval[1,2], 3),
      ADF_Diff_tau   = round(adf_d1@teststat[1], 3),
      ADF_Diff_cv5   = round(adf_d1@cval[1,2], 3),
      KPSS_Level     = round(kpss_lv@teststat, 3),
      KPSS_Level_cv5 = round(kpss_lv@cval[2], 3),
      KPSS_Diff      = round(kpss_d1@teststat, 3),
      Classification = classification
    )
  })
  df_out <- do.call(rbind, rows)
  cat("\n---", label, "---\n"); print(df_out)
  write.csv(df_out, file.path(outputs_dir, paste0("unitroot_", label, ".csv")), row.names = FALSE)
  df_out
}

ur_level <- run_unitroot_battery(merged,    "level")
ur_log   <- run_unitroot_battery(merged_log,"log")
ur_bc    <- run_unitroot_battery(merged_bc, "boxcox")

# Comparison summary
summarize_clean <- function(df, label) {
  n_clean <- sum(df$Classification != "AMBIGUOUS - tests disagree")
  n_I0 <- sum(df$Classification == "I(0) - both agree")
  n_I1 <- sum(df$Classification == "I(1) - both agree")
  data.frame(Transformation = label, N_clean = n_clean, N_I0 = n_I0,
             N_I1 = n_I1, N_ambiguous = 5 - n_clean)
}
transform_comparison <- rbind(
  summarize_clean(ur_level, "level"),
  summarize_clean(ur_log,   "log"),
  summarize_clean(ur_bc,    "boxcox")
)
cat("\n=== TRANSFORMATION COMPARISON ===\n"); print(transform_comparison)
write.csv(transform_comparison, file.path(outputs_dir, "transformation_comparison.csv"), row.names = FALSE)


