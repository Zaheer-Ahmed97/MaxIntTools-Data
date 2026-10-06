# ============================================================
# STEP 14: ARIMA + ETS + ARIMAX (standalone benchmarks)
# ============================================================
cat("\n=== STEP 10: ARIMA / ETS / ARIMAX ===\n")
# Define N from the merged data loaded in Part 1
N          <- nrow(merged)          # 112 observations
test_vars   <- c("QIM","FUEL","ENERGY","WHEAT","GAS")
test_labels <- c("QIM","Fuel","Electricity","Wheat","Gas")
train_n    <- floor(0.8*N)
y_train    <- merged$QIM[1:train_n]
y_test     <- merged$QIM[(train_n+1):N]
n_test     <- length(y_test)
test_dates <- merged$date[(train_n+1):N]
cat("Train:",train_n,"| Test:",n_test,"\n")

arima_fit  <- auto.arima(y_train,seasonal=TRUE,stepwise=FALSE,
                         approximation=FALSE,ic="aic",trace=FALSE)
cat("ARIMA:",paste(arima_fit$arma,collapse=","),"\n")
arima_fc   <- forecast(arima_fit,h=n_test)
arima_pred <- as.numeric(arima_fc$mean)
arima_rmse <- rmse_fn(y_test,arima_pred)
arima_mae  <- mae_fn(y_test,arima_pred)
arima_mape <- mape_fn(y_test,arima_pred)
cat(sprintf("ARIMA  RMSE=%.5f MAE=%.5f MAPE=%.2f%%\n",arima_rmse,arima_mae,arima_mape))

tiff(file.path(plots_dir,"Fig7_ARIMA_Diagnostics.tiff"),
     width=14,height=8,units="in",res=600,compression="lzw")
par(mfrow=c(2,2),mar=c(4,4,3,1))
plot(residuals(arima_fit),col="#E63946",main="ARIMA Residuals",type="l",lwd=1.5,ylab="Residual")
abline(h=0,lty=2,col="grey50")
acf(residuals(arima_fit),lag.max=36,main="ACF of Residuals",col="#E63946",lwd=2)
pacf(residuals(arima_fit),lag.max=36,main="PACF of Residuals",col="#E63946",lwd=2)
qqnorm(residuals(arima_fit),main="Q-Q Plot"); qqline(residuals(arima_fit),col="#E63946",lwd=2)
dev.off(); cat("Saved: Fig7_ARIMA_Diagnostics.tiff\n")
lb_test<-Box.test(residuals(arima_fit),lag=12,type="Ljung-Box")
jb_test<-jarque.bera.test(residuals(arima_fit))
cat(sprintf("=== ARIMA Residual Diagnostics ===\n"))
cat(sprintf("Ljung-Box Q(12) = %.3f,  p = %.4f  [%s]\n",
            lb_test$statistic, lb_test$p.value,
            ifelse(lb_test$p.value > 0.05, "White noise CONFIRMED", "Serial correlation detected")))
cat(sprintf("Jarque-Bera     = %.3f,  p = %.6f  [%s]\n",
            jb_test$statistic, jb_test$p.value,
            ifelse(jb_test$p.value < 0.05,
                   "NON-GAUSSIAN: nonlinear structure in residuals => ML hybrid justified",
                   "Gaussian residuals")))

cat("\n=== NONLINEARITY DIAGNOSTICS — ALL SERIES ===\n")
jb_levels <- do.call(rbind, lapply(seq_along(test_vars), function(ii){
  x  <- merged[[test_vars[ii]]]
  jb <- jarque.bera.test(x)
  data.frame(Series=test_labels[ii], Transform="Log-level",
             JB_stat=round(jb$statistic,3), JB_pval=round(jb$p.value,6),
             Decision=ifelse(jb$p.value<0.05,"Non-Gaussian","Gaussian"))
}))
cat("JB test — log-level series:\n"); print(jb_levels)
jb_diffs <- do.call(rbind, lapply(seq_along(test_vars), function(ii){
  dx <- diff(merged[[test_vars[ii]]])
  jb <- jarque.bera.test(dx)
  data.frame(Series=paste0("D.",test_labels[ii]), Transform="1st diff",
             JB_stat=round(jb$statistic,3), JB_pval=round(jb$p.value,6),
             Decision=ifelse(jb$p.value<0.05,"Non-Gaussian","Gaussian"))
}))
cat("JB test — first differences:\n"); print(jb_diffs)
arima_resid_clean <- as.numeric(residuals(arima_fit))
arima_resid_clean <- arima_resid_clean[!is.na(arima_resid_clean)]
cat("\nBDS Nonlinearity Test on ARIMA Residuals\n")
cat("(Brock, Dechert & Scheinkman 1996 — null: i.i.d.)\n")
eps_bds <- sd(arima_resid_clean) * c(0.5, 1.0, 1.5, 2.0)
bds_arima <- tryCatch(bds.test(arima_resid_clean, m=5, eps=eps_bds),
                      error=function(e){ cat("BDS error:",e$message,"\n"); NULL })
if(!is.null(bds_arima)) print(bds_arima)
cat("\nBDS on all 5 log-level series:\n")
for(ii in seq_along(test_vars)){
  x   <- merged[[test_vars[ii]]]
  eps <- sd(x) * c(0.5, 1.0, 1.5, 2.0)
  bds <- tryCatch(bds.test(x, m=4, eps=eps), error=function(e) NULL)
  cat(sprintf("\n%s:\n", test_labels[ii]))
  if(!is.null(bds)) print(bds) else cat("  (could not compute)\n")
}
linearity_table <- rbind(jb_levels, jb_diffs)
write.csv(linearity_table, file.path(outputs_dir,"linearity_diagnostics_full.csv"), row.names=FALSE)
cat("\nSaved: linearity_diagnostics_full.csv\n")
tiff(file.path(plots_dir,"Fig_Linearity_QQ.tiff"),width=16, height=10, units="in", res=300, compression="lzw")
par(mfrow=c(2,3), mar=c(4,4,3,1))
qq_data   <- c(lapply(test_vars, function(v) merged[[v]]), list(arima_resid_clean))
qq_labels2 <- c(test_labels, "ARIMA Residuals")
qq_cols   <- c("#E63946","#2A9D8F","#264653","#E9C46A","#F4A261","#9B5DE5")
for(ii in seq_along(qq_labels2)){
  x <- qq_data[[ii]]
  qqnorm(x, main=paste("Q-Q:", qq_labels2[ii]), col=qq_cols[ii], pch=19, cex=0.6)
  qqline(x, col="black", lwd=2, lty=2)
  jb_q <- jarque.bera.test(x)
  legend("topleft", bty="n", cex=0.85,
         legend=sprintf("JB=%.1f  p=%.4f", jb_q$statistic, jb_q$p.value))
}
dev.off(); cat("Saved: Fig_Linearity_QQ.tiff\n")
cat(sprintf("Ljung-Box p=%.4f | Jarque-Bera p=%.4f\n",lb_test$p.value,jb_test$p.value))

library(ggplot2)

pvals <- matrix(
  c(0.9337, 0.5254, 0.2590, 0.0252,
    0.0280, 0.8702, 0.7662, 0.0930,
    0.0465, 0.9925, 0.9508, 0.1885,
    0.4469, 0.8508, 0.6743, 0.3218),
  nrow = 4, ncol = 4, byrow = TRUE
)

m_vals   <- 2:5
eps_vals <- c(0.0414, 0.0827, 0.1241, 0.1654)
eps_labels <- c("0.0414\n(0.5\u03c3)", "0.0827\n(1.0\u03c3)",
                "0.1241\n(1.5\u03c3)", "0.1654\n(2.0\u03c3)")

df <- expand.grid(m = m_vals, eps_idx = 1:4)
df$pvalue <- as.vector(pvals)
df$m_fact   <- factor(df$m, levels = m_vals)
df$eps_fact <- factor(df$eps_idx, levels = 1:4, labels = eps_labels)

df$sig <- with(df, ifelse(pvalue < 0.01, "**",
                          ifelse(pvalue < 0.05, "*", "")))
df$label <- sprintf("%.3f%s", df$pvalue, df$sig)
df$bold  <- df$sig != ""
df$text_col <- with(df, ifelse(pvalue < 0.01 | pvalue > 0.6, "white", "black"))

df$log_p <- log10(df$pvalue)
log_thresh <- log10(0.05)

p <- ggplot(df, aes(x = eps_fact, y = m_fact, fill = log_p)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = label, color = text_col,
                fontface = ifelse(bold, "bold", "plain")),
            size = 4.2) +
  scale_color_manual(values = c("black" = "black", "white" = "white"),
                     guide = "none") +
  scale_fill_gradient2(
    low    = "#2166AC",
    mid    = "#F7F7F7",
    high   = "#B2182B",
    midpoint = log_thresh,
    limits  = c(log10(0.001), log10(1)),
    name    = "p-value\n(log scale)",
    breaks  = log10(c(0.001, 0.01, 0.05, 0.10, 0.50, 1.00)),
    labels  = c("0.001", "0.01", "0.05", "0.10", "0.50", "1.00"),
    guide   = guide_colorbar(
      barwidth = 0.9, barheight = 8,
      frame.colour = "black", ticks.colour = "black"
    )
  ) +
  labs(
    title = "BDS test on ARIMA residuals",
    subtitle = "Null: residuals are i.i.d.",
    x = expression("Proximity threshold  " * epsilon),
    y = "Embedding dimension"
  ) +
  coord_fixed() +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", hjust = 0.5, size = 13),
    plot.subtitle    = element_text(hjust = 0.5, color = "grey30"),
    axis.text.x      = element_text(size = 10),
    axis.text.y      = element_text(size = 11),
    panel.grid       = element_blank(),
    legend.position  = "right"
  )

p <- p + annotate(
  "text", x = 2.5, y = 0.2,
  label = "* p < 0.05    ** p < 0.01\nRejections are sparse (3 of 16) and scattered; no monotonic pattern across m.",
  size = 3, fontface = "italic", color = "grey30", hjust = 0.5
)

ggsave("Fig_BDS_residuals_heatmap.png", plot = p,
       width = 7.2, height = 5.0, dpi = 300, bg = "white")

ggsave("Fig_BDS_residuals_heatmap.tiff", plot = p,
       width = 7.2, height = 5.0, dpi = 300, compression = "lzw", bg = "white")

print(p)

y_ts_tr <- ts(y_train,frequency=12)
ets_fit  <- ets(y_ts_tr)
cat("ETS:",ets_fit$method,"\n")
ets_fc   <- forecast(ets_fit,h=n_test)
ets_pred <- as.numeric(ets_fc$mean)
ets_rmse <- rmse_fn(y_test,ets_pred)
ets_mae  <- mae_fn(y_test,ets_pred)
ets_mape <- mape_fn(y_test,ets_pred)
cat(sprintf("ETS    RMSE=%.5f MAE=%.5f MAPE=%.2f%%\n",ets_rmse,ets_mae,ets_mape))

xreg_tr <- as.matrix(merged[1:train_n,c("FUEL","ENERGY","WHEAT","GAS")])
xreg_te <- as.matrix(merged[(train_n+1):N,c("FUEL","ENERGY","WHEAT","GAS")])
arimax_fit  <- auto.arima(y_train,xreg=xreg_tr,seasonal=TRUE,stepwise=FALSE,approximation=FALSE,ic="aic")
arimax_fc   <- forecast(arimax_fit,xreg=xreg_te,h=n_test)
arimax_pred <- as.numeric(arimax_fc$mean)
arimax_rmse <- rmse_fn(y_test,arimax_pred)
arimax_mae  <- mae_fn(y_test,arimax_pred)
arimax_mape <- mape_fn(y_test,arimax_pred)
cat(sprintf("ARIMAX RMSE=%.5f MAE=%.5f MAPE=%.2f%%\n",arimax_rmse,arimax_mae,arimax_mape))
arimax_coef_raw <- as.data.frame(summary(arimax_fit)$coef)
arimax_coef_raw$Variable <- rownames(arimax_coef_raw)
arimax_coef_raw$AIC_model <- AIC(arimax_fit)
arimax_coef_raw$BIC_model <- BIC(arimax_fit)
write.csv(arimax_coef_raw, file.path(outputs_dir,"arimax_coefficients_full.csv"), row.names=FALSE)
cat("Saved: arimax_coefficients_full.csv (coef + SE + t + AIC/BIC)\n")
write.csv(data.frame(AIC=AIC(arimax_fit),BIC=BIC(arimax_fit)),
          file.path(outputs_dir,"arimax_coefficients.csv"),row.names=FALSE)
file.path(outputs_dir,"arimax_coefficients.csv",row.names=FALSE)

# ============================================================
# STEP 15: ML FEATURE MATRIX - ALIGNED TO ARIMA TEST DATES
# ============================================================
cat("\n=== STEP 11: ML FEATURE MATRIX (aligned to ARIMA test dates) ===\n")
feat_df   <- create_features(merged,lags=6)
feat_df   <- feat_df[complete.cases(feat_df),]
feat_cols <- grep("_lag|month|trend",names(feat_df),value=TRUE)
X_feat    <- as.matrix(feat_df[,feat_cols])
y_feat    <- feat_df$QIM

train_ml_idx <- which(feat_df$date < min(test_dates))
test_ml_idx  <- which(feat_df$date %in% test_dates)
X_train_ml <- X_feat[train_ml_idx,]; y_train_ml <- y_feat[train_ml_idx]
X_test_ml  <- X_feat[test_ml_idx,];  y_test_ml  <- y_feat[test_ml_idx]
td_ml <- feat_df$date[test_ml_idx]
n_tr_ml <- length(train_ml_idx); n_te_ml <- length(test_ml_idx)

cat(sprintf("ARIMA test: N=%d | %s to %s\n",length(test_dates),
            as.character(min(test_dates)),as.character(max(test_dates))))
cat(sprintf("ML test   : N=%d | %s to %s\n",n_te_ml,
            as.character(min(td_ml)),as.character(max(td_ml))))

# ============================================================
# STEP 16: STANDALONE ML
# ============================================================
cat("\n=== STEP 12a: RANDOM FOREST ===\n")
set.seed(42)
rf_fit  <- randomForest(x=X_train_ml,y=y_train_ml,ntree=500,
                        mtry=floor(sqrt(ncol(X_train_ml))),
                        importance=TRUE,nodesize=3)
rfp     <- predict(rf_fit,X_test_ml)
rf_rmse <- rmse_fn(y_test_ml,rfp); rf_mae<-mae_fn(y_test_ml,rfp); rf_mape<-mape_fn(y_test_ml,rfp)
cat(sprintf("Random Forest  RMSE=%.5f MAE=%.5f MAPE=%.2f%%\n",rf_rmse,rf_mae,rf_mape))
imp_df<-data.frame(Variable=rownames(importance(rf_fit)),Importance=importance(rf_fit)[,1]) %>%
  arrange(desc(Importance)) %>% head(20)
write.csv(imp_df,file.path(outputs_dir,"rf_importance.csv"),row.names=FALSE)
p8<-ggplot(imp_df,aes(x=reorder(Variable,Importance),y=Importance))+
  geom_col(fill="#1D6A96",width=0.7)+coord_flip()+
  labs(title="Random Forest Variable Importance",x=NULL,y="% Increase in MSE")+theme_pub(11)
save_tiff(p8,"Fig8_RF_Importance")

cat("\n=== STEP 12b: GRADIENT BOOSTING ===\n")
train_gbm<-as.data.frame(cbind(y=y_train_ml,X_train_ml))
set.seed(42)
gbm_fit<-gbm(y~.,data=train_gbm,distribution="gaussian",n.trees=500,
             interaction.depth=3,shrinkage=0.05,bag.fraction=0.80,cv.folds=5,verbose=FALSE)
best_iter<-gbm.perf(gbm_fit,method="cv",plot.it=FALSE)
cat("GBM best trees:",best_iter,"\n")
gp<-predict(gbm_fit,as.data.frame(X_test_ml),n.trees=best_iter)
gbm_rmse<-rmse_fn(y_test_ml,gp); gbm_mae<-mae_fn(y_test_ml,gp); gbm_mape<-mape_fn(y_test_ml,gp)
cat(sprintf("GBM    RMSE=%.5f MAE=%.5f MAPE=%.2f%%\n",gbm_rmse,gbm_mae,gbm_mape))
gbm_imp<-summary(gbm_fit,plotit=FALSE) %>% arrange(desc(rel.inf)) %>% head(20)
write.csv(gbm_imp,file.path(outputs_dir,"gbm_importance.csv"),row.names=FALSE)

# ============================================================
# STEP 17: FOUR HYBRID MODELS
# ============================================================
arima_resid_train <- as.numeric(residuals(arima_fit))
ets_resid_train   <- as.numeric(residuals(ets_fit))
arima_fc_vec      <- as.numeric(arima_fc$mean)
ets_fc_vec        <- as.numeric(ets_fc$mean)
nh                <- min(length(arima_fc_vec),n_te_ml)

cat("\n=== STEP 13a: H1 - ARIMA-GBM HYBRID ===\n")
cat("ARIMA captures dominant linear autocorrelation;\n")
cat("GBM corrects nonlinear residuals efficiently on small N.\n")
h1<-fit_gbm_corrector(arima_resid_train,X_train_ml,X_test_ml)
nh_h1<-min(nh,length(h1$pred)); h1_pred<-arima_fc_vec[1:nh_h1]+h1$pred[1:nh_h1]
h1_rmse<-rmse_fn(y_test_ml[1:nh_h1],h1_pred)
h1_mae <-mae_fn(y_test_ml[1:nh_h1],h1_pred)
h1_mape<-mape_fn(y_test_ml[1:nh_h1],h1_pred)
cat(sprintf("H1 ARIMA-GBM  RMSE=%.5f MAE=%.5f MAPE=%.2f%%\n",h1_rmse,h1_mae,h1_mape))

cat("\n=== STEP 13b: H2 - ARIMA-RF HYBRID ===\n")
cat("RF corrector has lower variance than GBM;\n")
cat("more robust when residual nonlinear patterns are weak.\n")
h2<-fit_rf_corrector(arima_resid_train,X_train_ml,X_test_ml)
nh_h2<-min(nh,length(h2$pred)); h2_pred<-arima_fc_vec[1:nh_h2]+h2$pred[1:nh_h2]
h2_rmse<-rmse_fn(y_test_ml[1:nh_h2],h2_pred)
h2_mae <-mae_fn(y_test_ml[1:nh_h2],h2_pred)
h2_mape<-mape_fn(y_test_ml[1:nh_h2],h2_pred)
cat(sprintf("H2 ARIMA-RF   RMSE=%.5f MAE=%.5f MAPE=%.2f%%\n",h2_rmse,h2_mae,h2_mape))

cat("\n=== STEP 13c: H3 - STL-GBM HYBRID ===\n")
cat("STL separates trend/season/remainder cleanly;\n")
cat("GBM models the remainder (pure nonlinear component).\n")
y_ts_full <- ts(merged$QIM,
                start=c(as.numeric(format(merged$date[1],"%Y")),
                        as.numeric(format(merged$date[1],"%m"))),
                frequency=12)
stl_fit      <- stl(y_ts_full,s.window="periodic",robust=TRUE)
stl_comps    <- as.data.frame(stl_fit$time.series)
sa_series    <- y_ts_full - stl_comps$seasonal
sa_train     <- window(sa_series,end=time(sa_series)[train_n])
stl_arima    <- auto.arima(sa_train,seasonal=FALSE,stepwise=FALSE,
                           approximation=FALSE,ic="aic",trace=FALSE)
stl_fc_sa    <- as.numeric(forecast(stl_arima,h=n_test)$mean)
seas_test    <- as.numeric(stl_comps$seasonal)[(train_n+1):N]
stl_fc_full  <- stl_fc_sa + seas_test
stl_resid_tr <- as.numeric(stl_arima$residuals)
stl_resid_tr <- stl_resid_tr[!is.na(stl_resid_tr)]
h3<-fit_gbm_corrector(stl_resid_tr,X_train_ml,X_test_ml)
nh_h3<-min(length(stl_fc_full),n_te_ml,length(h3$pred))
h3_pred<-stl_fc_full[1:nh_h3]+h3$pred[1:nh_h3]
h3_rmse<-rmse_fn(y_test_ml[1:nh_h3],h3_pred)
h3_mae <-mae_fn(y_test_ml[1:nh_h3],h3_pred)
h3_mape<-mape_fn(y_test_ml[1:nh_h3],h3_pred)
cat(sprintf("H3 STL-GBM    RMSE=%.5f MAE=%.5f MAPE=%.2f%%\n",h3_rmse,h3_mae,h3_mape))

cat("\n=== STEP 13d: H4 - ETS-GBM HYBRID ===\n")
cat("ETS handles level/trend/season via exponential smoothing;\n")
cat("GBM models remaining nonlinear dynamics.\n")
h4<-fit_gbm_corrector(ets_resid_train,X_train_ml,X_test_ml)
nh_h4<-min(length(ets_fc_vec),n_te_ml,length(h4$pred))
h4_pred<-ets_fc_vec[1:nh_h4]+h4$pred[1:nh_h4]
h4_rmse<-rmse_fn(y_test_ml[1:nh_h4],h4_pred)
h4_mae <-mae_fn(y_test_ml[1:nh_h4],h4_pred)
h4_mape<-mape_fn(y_test_ml[1:nh_h4],h4_pred)
cat(sprintf("H4 ETS-GBM    RMSE=%.5f MAE=%.5f MAPE=%.2f%%\n",h4_rmse,h4_mae,h4_mape))

# ============================================================
# STEP 18: 9-MODEL COMPARISON
# ============================================================
cat("\n=== STEP 14: 9-MODEL COMPARISON ===\n")

comparison <- data.frame(
  Model = c("ARIMA","ETS","ARIMAX","Random Forest","Gradient Boosting",
            "H1: ARIMA-GBM","H2: ARIMA-RF","H3: STL-GBM","H4: ETS-GBM"),
  Type  = c("Econometric","Econometric","Econometric","ML","ML",
            "Hybrid","Hybrid","Hybrid","Hybrid"),
  RMSE  = c(arima_rmse,ets_rmse,arimax_rmse,rf_rmse,gbm_rmse,
            h1_rmse,h2_rmse,h3_rmse,h4_rmse),
  MAE   = c(arima_mae,ets_mae,arimax_mae,rf_mae,gbm_mae,
            h1_mae,h2_mae,h3_mae,h4_mae),
  MAPE  = c(arima_mape,ets_mape,arimax_mape,rf_mape,gbm_mape,
            h1_mape,h2_mape,h3_mape,h4_mape)
)
comparison <- comparison[order(comparison$RMSE),]
comparison$Rank <- seq_len(nrow(comparison))
cat(sprintf("\n9-MODEL COMPARISON (N_test=%d, all same period):\n",n_te_ml))
print(comparison[,c("Rank","Model","Type","RMSE","MAE","MAPE")])
write.csv(comparison,file.path(outputs_dir,"model_comparison_full.csv"),row.names=FALSE)

cat("\n=== DIEBOLD-MARIANO SIGNIFICANCE TESTS ===\n")
cat("H0: Competitor equally accurate to STL-GBM (H3)\n")
e_stlgbm <- y_test_ml[1:nh_h3] - h3_pred
dm_comps <- list(
  "ARIMA"         = y_test        - arima_pred[1:length(y_test)],
  "ETS"           = y_test        - ets_pred[1:length(y_test)],
  "ARIMAX"        = y_test        - arimax_pred[1:length(y_test)],
  "Random Forest" = y_test_ml     - rfp,
  "GBM"           = y_test_ml     - gp,
  "H1_ARIMA_GBM"  = y_test_ml[1:nh_h1] - h1_pred,
  "H2_ARIMA_RF"   = y_test_ml[1:nh_h2] - h2_pred,
  "H4_ETS_GBM"    = y_test_ml[1:nh_h4] - h4_pred
)
dm_results <- do.call(rbind, lapply(names(dm_comps), function(nm){
  e2 <- dm_comps[[nm]]
  n  <- min(length(e_stlgbm), length(e2))
  dm <- tryCatch(
    dm.test(e_stlgbm[1:n], e2[1:n], h=1, power=2, alternative="less"),
    error=function(e) list(statistic=NA_real_, p.value=NA_real_))
  data.frame(Competitor=nm,
             DM_stat  =round(dm$statistic,3),
             p_value  =round(dm$p.value,  4),
             Decision =ifelse(!is.na(dm$p.value)&dm$p.value<0.05,
                              "STL-GBM significantly better","Not significant"))
}))
cat("Diebold-Mariano Results:\n"); print(dm_results)
write.csv(dm_results, file.path(outputs_dir,"diebold_mariano_tests.csv"), row.names=FALSE)
cat("Saved: diebold_mariano_tests.csv\n")

# ============================================================
# STEP 19: FORECAST PLOTS
# ============================================================
cat("\n=== STEP 15: FORECAST PLOTS ===\n")
n_plt <- n_te_ml
act_df <- data.frame(date=td_ml[1:n_plt],value=y_test_ml[1:n_plt],model="Actual")

p9a<-ggplot(rbind(act_df,
                  data.frame(date=td_ml[1:n_plt],value=arima_pred[1:n_plt],model="ARIMA"),
                  data.frame(date=td_ml[1:n_plt],value=ets_pred[1:n_plt],model="ETS"),
                  data.frame(date=td_ml[1:n_plt],value=arimax_pred[1:n_plt],model="ARIMAX")),
            aes(x=date,y=value,color=model,linewidth=model))+geom_line()+
  scale_color_manual(values=c("Actual"="black","ARIMA"="#457B9D","ETS"="#2A9D8F","ARIMAX"="#E9C46A"))+
  scale_linewidth_manual(values=c("Actual"=1.2,"ARIMA"=0.9,"ETS"=0.9,"ARIMAX"=0.9),guide="none")+
  labs(title="Econometric Models vs Actual QIM",x="Date",y="QIM",color=NULL)+theme_pub(11)
save_tiff(p9a,"Fig9a_Forecast_Econometric")

p9b<-ggplot(rbind(act_df,
                  data.frame(date=td_ml[1:n_plt],value=rfp[1:n_plt],model="Random Forest"),
                  data.frame(date=td_ml[1:n_plt],value=gp[1:n_plt],model="Gradient Boosting")),
            aes(x=date,y=value,color=model,linewidth=model))+geom_line()+
  scale_color_manual(values=c("Actual"="black","Random Forest"="#E63946","Gradient Boosting"="#2A9D8F"))+
  scale_linewidth_manual(values=c("Actual"=1.2,"Random Forest"=0.9,"Gradient Boosting"=0.9),guide="none")+
  labs(title="Standalone ML Models vs Actual QIM",x="Date",y="QIM",color=NULL)+theme_pub(11)
save_tiff(p9b,"Fig9b_Forecast_ML")

n_hyb <- min(nh_h1,nh_h2,nh_h3,nh_h4)
p9c<-ggplot(rbind(
  data.frame(date=td_ml[1:n_hyb],value=y_test_ml[1:n_hyb],model="Actual"),
  data.frame(date=td_ml[1:n_hyb],value=h1_pred[1:n_hyb],model="H1: ARIMA-GBM"),
  data.frame(date=td_ml[1:n_hyb],value=h2_pred[1:n_hyb],model="H2: ARIMA-RF"),
  data.frame(date=td_ml[1:n_hyb],value=h3_pred[1:n_hyb],model="H3: STL-GBM"),
  data.frame(date=td_ml[1:n_hyb],value=h4_pred[1:n_hyb],model="H4: ETS-GBM")),
  aes(x=date,y=value,color=model,linetype=model,linewidth=model))+geom_line()+
  scale_color_manual(values=c("Actual"="black","H1: ARIMA-GBM"="#1D6A96",
                              "H2: ARIMA-RF"="#E63946","H3: STL-GBM"="#2A9D8F","H4: ETS-GBM"="#9B5DE5"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dotdash","longdash"))+
  scale_linewidth_manual(values=c(1.2,0.9,0.9,0.9,0.9),guide="none")+
  labs(title="All 4 Hybrid Models vs Actual QIM",
       subtitle="H1=ARIMA-GBM | H2=ARIMA-RF | H3=STL-GBM | H4=ETS-GBM",
       x="Date",y="QIM",color=NULL,linetype=NULL)+theme_pub(11)
save_tiff(p9c,"Fig9c_Forecast_Hybrids",width=12,height=6)

comp_long <- comparison %>%
  dplyr::select(Model,Type,RMSE,MAE,MAPE,Rank) %>%
  tidyr::pivot_longer(c(RMSE,MAE,MAPE),names_to="Metric",values_to="Value") %>%
  mutate(Model=factor(Model,levels=rev(comparison$Model)),
         Metric=factor(Metric,levels=c("RMSE","MAE","MAPE")),
         FillC=case_when(
           Rank==1           ~ "Best",
           Type=="Hybrid"    ~ "Other Hybrid",
           Type=="ML"        ~ "Standalone ML",
           TRUE              ~ "Econometric"))
p10<-ggplot(comp_long,aes(x=Model,y=Value,fill=FillC))+
  geom_col(width=0.68)+
  geom_text(aes(label=ifelse(Metric=="MAPE",sprintf("%.2f%%",Value),sprintf("%.4f",Value))),
            hjust=-0.08,size=2.6,color="grey30")+
  facet_wrap(~Metric,scales="free_x",nrow=1)+
  scale_fill_manual(values=c("Best"="#1D6A96","Other Hybrid"="#6BAED6",
                             "Standalone ML"="#E63946","Econometric"="#A8C7E0"),name=NULL)+
  coord_flip()+scale_y_continuous(expand=expansion(mult=c(0,0.28)))+
  labs(title="Complete 9-Model Forecasting Performance Comparison",
       subtitle=paste0("Same test set N=",n_te_ml," | Lower = better | Dark blue = best model"),
       x=NULL,y="Error metric value")+
  theme_pub(10)+theme(legend.position="bottom",panel.spacing=unit(1.2,"lines"))
save_tiff(p10,"Fig10_ModelComparison",width=15,height=8)

# ============================================================
# STEP 20: SUPPLEMENTARY FIGURES
# ============================================================
cat("\n=== STEP 16: SUPPLEMENTARY FIGURES ===\n")

sc_df<-merged %>% dplyr::select(date,QIM,FUEL,ENERGY,WHEAT,GAS) %>%
  tidyr::pivot_longer(-c(date,QIM),names_to="Commodity",values_to="CPrice") %>%
  mutate(Commodity=factor(Commodity,levels=c("FUEL","ENERGY","WHEAT","GAS"),
                          labels=c("Fuel","Electricity","Wheat","Gas")),
         Period=cut(as.numeric(date),breaks=4,labels=c("2016-2018","2019-2020","2021-2022","2023-2025")))
p11<-ggplot(sc_df[!is.na(sc_df$CPrice),],aes(x=CPrice,y=QIM,color=Period))+
  geom_point(alpha=0.65,size=2.2)+
  geom_smooth(aes(group=1),method="lm",se=TRUE,color="black",linetype="dashed",linewidth=1)+
  facet_wrap(~Commodity,scales="free_x",ncol=2)+
  scale_color_manual(values=c("#2166AC","#74ADD1","#FD8D3C","#E63946"),name="Period")+
  labs(title="Scatter Plots: Industrial Production vs Commodity Prices",
       x="Commodity Price",y="QIM")+theme_pub(12)
save_tiff(p11,"Fig11_ScatterPlots",width=12,height=10)

corr_mat<-cor(merged[,c("QIM","FUEL","ENERGY","WHEAT","GAS")],use="complete.obs")
colnames(corr_mat)<-rownames(corr_mat)<-c("QIM","Fuel","Elec.","Wheat","Gas")
corr_melt<-melt(corr_mat)
p12<-ggplot(corr_melt,aes(Var1,Var2,fill=value))+geom_tile(color="white")+
  geom_text(aes(label=round(value,2)),size=5,fontface="bold",
            color=ifelse(abs(melt(corr_mat)$value)>0.5,"white","black"))+
  scale_fill_gradient2(low="#264653",mid="white",high="#E63946",midpoint=0,limits=c(-1,1),name="r")+
  labs(title="Pearson Correlation Matrix",x=NULL,y=NULL)+
  theme_pub(12)+theme(axis.text.x=element_text(angle=30,hjust=1))
save_tiff(p12,"Fig12_CorrelationHeatmap",width=8,height=7)

y_ts2<-ts(merged$QIM,
          start=c(as.numeric(format(merged$date[1],"%Y")),as.numeric(format(merged$date[1],"%m"))),
          frequency=12)
bp<-breakpoints(y_ts2~1)
tiff(file.path(plots_dir,"Fig13_StructuralBreaks.tiff"),width=14,height=6,units="in",res=600,compression="lzw")
par(mar=c(4,4,3,2))
plot(y_ts2,main="Structural Breaks in Industrial Production",ylab="QIM",xlab="Year",col="#264653",lwd=1.5)
lines(fitted(bp),col="#E63946",lwd=2)
legend("topleft",c("Observed QIM","Segmented Trend"),col=c("#264653","#E63946"),lty=1,lwd=c(1.5,2),bty="n")
dev.off(); cat("Saved: Fig13_StructuralBreaks.tiff\n")

win<-24
roll_df<-do.call(rbind,lapply((win+1):N,function(i){w<-(i-win+1):i;do.call(rbind,lapply(c("FUEL","ENERGY","WHEAT","GAS"),function(cv) data.frame(date=merged$date[i],Variable=cv,Corr=cor(merged$QIM[w],merged[[cv]][w],use="complete.obs"))))}))
roll_df$Variable<-factor(roll_df$Variable,levels=c("FUEL","ENERGY","WHEAT","GAS"),labels=c("Fuel","Electricity","Wheat","Gas"))
p14<-ggplot(roll_df,aes(x=date,y=Corr,color=Variable))+geom_line(linewidth=0.9)+
  geom_hline(yintercept=0,linetype="dashed",color="grey40")+
  scale_color_manual(values=c("#E63946","#2A9D8F","#E9C46A","#264653"),name="")+
  scale_x_date(date_breaks="1 year",date_labels="%Y")+
  labs(title="24-Month Rolling Correlations: Commodity Prices vs QIM",x="Date",y="Pearson r")+theme_pub(12)
save_tiff(p14,"Fig14_RollingCorrelations",width=12,height=6)

mg<-merged %>%
  mutate(QIM_g=(QIM/lag(QIM,12)-1)*100,FUEL_g=(FUEL/lag(FUEL,12)-1)*100,
         WHEAT_g=(WHEAT/lag(WHEAT,12)-1)*100,ENERGY_g=(ENERGY/lag(ENERGY,12)-1)*100) %>%
  filter(!is.na(QIM_g)) %>%
  tidyr::pivot_longer(c(QIM_g,FUEL_g,WHEAT_g,ENERGY_g),names_to="Variable",values_to="Growth") %>%
  mutate(Variable=factor(Variable,levels=c("QIM_g","FUEL_g","WHEAT_g","ENERGY_g"),
                         labels=c("Industrial Production","Fuel Prices","Wheat Prices","Electricity")))
p15<-ggplot(mg,aes(x=date,y=Growth,fill=Growth>0))+geom_col(width=28,show.legend=FALSE)+
  geom_hline(yintercept=0,color="black",linewidth=0.4)+facet_wrap(~Variable,ncol=1,scales="free_y")+
  scale_fill_manual(values=c("TRUE"="#2A9D8F","FALSE"="#E63946"))+
  scale_x_date(date_breaks="1 year",date_labels="%Y")+
  labs(title="Year-on-Year Growth Rates",x="Date",y="YoY Growth (%)")+theme_pub(11)
save_tiff(p15,"Fig15_GrowthRates",width=12,height=12)