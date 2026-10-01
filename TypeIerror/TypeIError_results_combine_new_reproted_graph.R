## =============================================================================
##  Empirical Type I error rates of item-fit / model-fit tests
##  Step 1: combine the simulation summary files into one Excel workbook
##  Step 2: publication-quality figure (PDF + PNG, 600 dpi)
## =============================================================================

## ---- 0. USER SETTINGS (edit only this block) -------------------------------
path        <- "C:/Users/ahmed/Desktop/Chapter 4 graphs and results/Type1_Error_Rate_Results/"
results_dir <- file.path(path, "TypeIError_results")   # folder with the CSVs
out_dir     <- file.path(path, "output")               # results are saved here
alpha       <- 0.05                                     # nominal level
fig_width   <- 11                                       # inches
fig_height  <- 5.5                                      # inches
fig_dpi     <- 600

## ---- 1. Packages -----------------------------------------------------------
pkgs <- c("ggplot2", "scales", "openxlsx")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) install.packages(missing_pkgs)
has_ragg <- requireNamespace("ragg", quietly = TRUE)   # optional (nicer PNG)

## ---- 2. Find and read the files --------------------------------------------
## Only files named like "..session1_I200_J50_summary.csv" are used;
## any other CSV in the folder (e.g. FINAL_optimized_summary.csv) is ignored.
files <- list.files(results_dir,
                    pattern = "session[0-9]+_I[0-9]+_J[0-9]+_summary\\.csv$",
                    full.names = TRUE)
if (length(files) == 0)
  stop("No session*_I*_J*_summary.csv files found in: ", results_dir)
cat("Files used:\n"); print(basename(files))

read_one <- function(f) {
  d  <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  nm <- basename(f)
  d$Session     <- as.integer(sub(".*session([0-9]+)_.*", "\\1", nm))
  d$I           <- as.integer(sub(".*_I([0-9]+)_J.*",     "\\1", nm))
  d$J           <- as.integer(sub(".*_J([0-9]+)_.*",      "\\1", nm))
  d$Source_File <- nm
  d
}
dat_list <- lapply(files, read_one)

## keep only the columns shared by all files, so rbind() can never fail
common   <- Reduce(intersect, lapply(dat_list, names))
needed   <- c("Test", "Type1_Error", "Lower_CI", "Upper_CI")
if (!all(needed %in% common))
  stop("Missing column(s): ", paste(setdiff(needed, common), collapse = ", "))
combined <- do.call(rbind, lapply(dat_list, function(d) d[, common]))

## column order and row order
front    <- c("Session", "I", "J", "Test")
combined <- combined[, c(front, setdiff(names(combined), front))]
test_order <- unique(dat_list[[1]]$Test)
combined <- combined[order(combined$I, combined$J,
                           match(combined$Test, test_order)), ]
rownames(combined) <- NULL
str(combined)

## ---- 3. Excel workbook -----------------------------------------------------
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

combined$Condition <- paste0("I=", combined$I, ", J=", combined$J)
cond_order <- unique(combined$Condition)          # already sorted by I, J
wide <- data.frame(Test = test_order, stringsAsFactors = FALSE)
for (cc in cond_order) {
  sub_d <- combined[combined$Condition == cc, ]
  wide[[cc]] <- sub_d$Type1_Error[match(wide$Test, sub_d$Test)]
}
combined$Condition <- NULL

wb <- openxlsx::createWorkbook()
hs <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9E1F2",
                            border = "Bottom", halign = "center")
openxlsx::addWorksheet(wb, "Combined_long")
openxlsx::writeData(wb, "Combined_long", combined, headerStyle = hs)
openxlsx::setColWidths(wb, "Combined_long", cols = seq_along(combined), widths = "auto")
openxlsx::freezePane(wb, "Combined_long", firstRow = TRUE)
openxlsx::addWorksheet(wb, "TypeI_wide")
openxlsx::writeData(wb, "TypeI_wide", wide, headerStyle = hs)
openxlsx::setColWidths(wb, "TypeI_wide", cols = seq_along(wide), widths = "auto")
openxlsx::freezePane(wb, "TypeI_wide", firstRow = TRUE)
xlsx_file <- file.path(out_dir, "TypeI_error_combined.xlsx")
openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE)
cat("Saved:", xlsx_file, "\n")

## ---- 4. Data for the plot --------------------------------------------------
R_reps <- max(combined$N_Valid, na.rm = TRUE)          # replications (1000)
if (!is.finite(R_reps)) R_reps <- 1000
mc_hw  <- stats::qnorm(0.975) * sqrt(alpha * (1 - alpha) / R_reps)

test_labels <- c(
  Andersen_LR    = "Andersen\nLR",
  Martin_Lof     = "Martin-\nL\u00f6f",
  LMuo           = "LMuo",
  maxLM          = "maxLM",
  T10            = "T10",
  T11            = "T11",
  M2             = "M2",
  REMAXINT_P2_Q2 = "REMAXINT",
  E_REMI_P2_Q2   = "E-REMI"
)
## any test not in the list above keeps its own name
extra <- setdiff(test_order, names(test_labels))
test_labels <- c(test_labels[names(test_labels) %in% test_order],
                 stats::setNames(extra, extra))

plot_df <- combined
plot_df$Test <- factor(plot_df$Test, levels = names(test_labels),
                       labels = unname(test_labels))
cond_lab <- paste0("I = ", plot_df$I, ", J = ", plot_df$J)
plot_df$Condition <- factor(cond_lab,
                            levels = unique(cond_lab[order(plot_df$I, plot_df$J)]))
plot_df$Lower_CI <- pmax(plot_df$Lower_CI, 0)          # truncate CI at 0

n_cond <- nlevels(plot_df$Condition)
cols   <- rep(c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00",
                "#56B4E9", "#F0E442", "#000000"), length.out = n_cond)
shapes <- rep(c(21, 24, 22, 23, 25), length.out = n_cond)
pd     <- ggplot2::position_dodge(width = 0.72)
y_top  <- max(plot_df$Upper_CI, 1.5 * alpha, na.rm = TRUE) * 1.03

## ---- 5. The figure ---------------------------------------------------------
p <- ggplot2::ggplot(plot_df,
                     ggplot2::aes(x = Test, y = Type1_Error,
                                  colour = Condition, shape = Condition, fill = Condition)) +
  ggplot2::geom_blank() +
  ## Bradley's (1978) liberal criterion [0.5*alpha, 1.5*alpha]
  ggplot2::annotate("rect", xmin = -Inf, xmax = Inf,
                    ymin = 0.5 * alpha, ymax = 1.5 * alpha,
                    fill = "grey88", alpha = 0.7) +
  ## 95% Monte Carlo interval around nominal alpha
  ggplot2::annotate("rect", xmin = -Inf, xmax = Inf,
                    ymin = alpha - mc_hw, ymax = alpha + mc_hw,
                    fill = "grey72", alpha = 0.7) +
  ggplot2::geom_hline(yintercept = alpha, linetype = "dashed",
                      linewidth = 0.6, colour = "black") +
  ggplot2::geom_vline(xintercept = seq(1.5, nlevels(plot_df$Test) - 0.5, 1),
                      colour = "grey80", linewidth = 0.3) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = Lower_CI, ymax = Upper_CI),
                         position = pd, width = 0, linewidth = 0.6) +
  ggplot2::geom_point(position = pd, size = 2.4, stroke = 0.45,
                      colour = "black") +
  ggplot2::scale_colour_manual(values = cols) +
  ggplot2::scale_fill_manual(values = cols) +
  ggplot2::scale_shape_manual(values = shapes) +
  ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.05),
                              labels = scales::label_number(accuracy = 0.01),
                              expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
  ggplot2::coord_cartesian(ylim = c(0, y_top)) +
  ggplot2::labs(x = "Test", y = "Empirical Type I error rate",
                colour = "Data sizes", shape = "Data sizes", fill = "Data sizes") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(face = "bold", size = 13,
                                         margin = ggplot2::margin(t = 8)),
    axis.title.y = ggplot2::element_text(face = "bold", size = 13,
                                         margin = ggplot2::margin(r = 8)),
    axis.text.x  = ggplot2::element_text(face = "bold", size = 10.5,
                                         colour = "black", lineheight = 0.9),
    axis.text.y  = ggplot2::element_text(face = "bold", size = 11,
                                         colour = "black"),
    axis.line    = ggplot2::element_line(linewidth = 0.6, colour = "black"),
    axis.ticks   = ggplot2::element_line(linewidth = 0.6, colour = "black"),
    axis.ticks.length  = ggplot2::unit(0.15, "cm"),
    legend.position    = "right",
    legend.title       = ggplot2::element_text(face = "bold", size = 12),
    legend.text        = ggplot2::element_text(size = 10.5),
    legend.key.height  = ggplot2::unit(0.75, "cm"),
    legend.key.width   = ggplot2::unit(0.6, "cm"),
    legend.margin      = ggplot2::margin(l = 2),
    legend.box.background = ggplot2::element_rect(colour = "grey60", linewidth = 0.4),
    legend.box.margin  = ggplot2::margin(4, 6, 4, 6),
    panel.grid.major.y = ggplot2::element_line(colour = "grey92", linewidth = 0.3),
    plot.margin        = ggplot2::margin(8, 12, 8, 8)
  ) +
  ggplot2::guides(colour = ggplot2::guide_legend(
    ncol = 1, override.aes = list(size = 3, colour = "black")))

print(p)

## ---- 6. Save PDF (vector) and PNG (600 dpi) --------------------------------
pdf_file <- file.path(out_dir, "TypeI_error_plot.pdf")
png_file <- file.path(out_dir, "TypeI_error_plot.png")

ok_pdf <- tryCatch({
  ggplot2::ggsave(pdf_file, p, width = fig_width, height = fig_height,
                  device = grDevices::cairo_pdf); TRUE
}, error = function(e) FALSE)
if (!ok_pdf)   # fallback: standard PDF device
  ggplot2::ggsave(pdf_file, p, width = fig_width, height = fig_height,
                  device = "pdf", useDingbats = FALSE)

if (has_ragg) {
  ggplot2::ggsave(png_file, p, width = fig_width, height = fig_height,
                  dpi = fig_dpi, device = ragg::agg_png, bg = "white")
} else {
  ggplot2::ggsave(png_file, p, width = fig_width, height = fig_height,
                  dpi = fig_dpi, device = "png", bg = "white")
}
cat("Saved:", pdf_file, "\nSaved:", png_file, "\n")

## Optional TIFF (some journals require it) - remove the leading '#' to use:
# ggplot2::ggsave(file.path(out_dir, "TypeI_error_plot.tiff"), p,
#                 width = fig_width, height = fig_height, dpi = fig_dpi,
#                 device = "tiff", compression = "lzw", bg = "white")