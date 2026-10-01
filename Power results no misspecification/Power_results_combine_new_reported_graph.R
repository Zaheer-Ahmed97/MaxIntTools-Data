## =============================================================================
##  Empirical power of item-fit / model-fit tests
##  Step 1: combine the simulation summary files into one Excel workbook
##  Step 2: publication-quality figure (PDF + PNG, 600 dpi)
## =============================================================================

## ---- 0. USER SETTINGS (edit only this block) -------------------------------
path        <- "C:/Users/ahmed/Desktop/Chapter 4 graphs and results/Power_Results_True_Case"
results_dir <- file.path(path, "Power_results")    # folder with the CSVs
out_dir     <- file.path(path, "output")           # results are saved here
target_pow  <- 0.80                                # reference line (conventional)
fig_width   <- 11                                  # inches
fig_height  <- 8                                   # inches (two stacked panels)
fig_dpi     <- 600

## ---- 1. Packages -----------------------------------------------------------
pkgs <- c("ggplot2", "scales", "openxlsx")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) install.packages(missing_pkgs)
has_ragg <- requireNamespace("ragg", quietly = TRUE)

## ---- 2. Find and read the files --------------------------------------------
## Only files like "Power_session1_I200_J50_ALL_conditions_summary.csv" are
## used; anything else in the folder (e.g. a FINAL_... file) is ignored.
files <- list.files(results_dir,
                    pattern = "Power_session[0-9]+_I[0-9]+_J[0-9]+_.*summary\\.csv$",
                    full.names = TRUE)
if (length(files) == 0)
  stop("No Power_session*_I*_J*_*summary.csv files found in: ", results_dir)
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

common <- Reduce(intersect, lapply(dat_list, names))
needed <- c("Condition", "Test", "Statistical_Power", "Lower_CI", "Upper_CI")
if (!all(needed %in% common))
  stop("Missing column(s): ", paste(setdiff(needed, common), collapse = ", "))
combined <- do.call(rbind, lapply(dat_list, function(d) d[, common]))

## REMAXINT / E-REMI carry the condition in their name (e.g. REMAXINT_P3_Q3).
## Keep the original name, and add a harmonised one so the tests line up.
combined$Test_Original <- combined$Test
combined$Test <- sub("_P[0-9]+_Q[0-9]+$", "", combined$Test)

front    <- c("Session", "I", "J", "Condition", "Test")
combined <- combined[, c(front, setdiff(names(combined), front))]
test_order <- unique(combined$Test)
combined <- combined[order(combined$Condition, combined$I, combined$J,
                           match(combined$Test, test_order)), ]
rownames(combined) <- NULL
str(combined)

## ---- 3. Excel workbook -----------------------------------------------------
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## wide table: one row per Condition x Test, one column per data size
size_key <- paste0("I=", combined$I, ", J=", combined$J)
size_ord <- unique(size_key[order(combined$I, combined$J)])
wide <- unique(combined[, c("Condition", "Test")])
for (s in size_ord) {
  sub_d <- combined[size_key == s, ]
  wide[[s]] <- sub_d$Statistical_Power[match(paste(wide$Condition, wide$Test),
                                             paste(sub_d$Condition, sub_d$Test))]
}
rownames(wide) <- NULL

wb <- openxlsx::createWorkbook()
hs <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9E1F2",
                            border = "Bottom", halign = "center")
for (sh in list(list("Combined_long", combined), list("Power_wide", wide))) {
  openxlsx::addWorksheet(wb, sh[[1]])
  openxlsx::writeData(wb, sh[[1]], sh[[2]], headerStyle = hs)
  openxlsx::setColWidths(wb, sh[[1]], cols = seq_along(sh[[2]]), widths = "auto")
  openxlsx::freezePane(wb, sh[[1]], firstRow = TRUE)
}
xlsx_file <- file.path(out_dir, "Power_combined.xlsx")
openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE)
cat("Saved:", xlsx_file, "\n")

## ---- 4. Data for the plot --------------------------------------------------
test_labels <- c(
  Andersen_LR = "Andersen\nLR",
  Martin_Lof  = "Martin-\nL\u00f6f",
  LMuo        = "LMuo",
  maxLM       = "maxLM",
  T10         = "T10",
  T11         = "T11",
  M2          = "M2",
  REMAXINT    = "REMAXINT",
  E_REMI      = "E-REMI"
)
extra <- setdiff(test_order, names(test_labels))
test_labels <- c(test_labels[names(test_labels) %in% test_order],
                 stats::setNames(extra, extra))

plot_df <- combined
plot_df$Test <- factor(plot_df$Test, levels = names(test_labels),
                       labels = unname(test_labels))

size_lab <- paste0("I = ", plot_df$I, ", J = ", plot_df$J)
plot_df$DataSize <- factor(size_lab,
                           levels = unique(size_lab[order(plot_df$I, plot_df$J)]))

## panel labels: "P2_Q2" -> "Generating model: P = 2, Q = 2"
cond_lab <- if (all(c("true_P", "true_Q") %in% names(plot_df))) {
  paste0("Generating model: P = ", plot_df$true_P, ", Q = ", plot_df$true_Q)
} else plot_df$Condition
plot_df$Panel <- factor(cond_lab, levels = unique(cond_lab[order(plot_df$Condition)]))

plot_df$Lower_CI <- pmax(plot_df$Lower_CI, 0)     # truncate CI to [0, 1]
plot_df$Upper_CI <- pmin(plot_df$Upper_CI, 1)

n_size <- nlevels(plot_df$DataSize)
cols   <- rep(c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00",
                "#56B4E9", "#F0E442", "#000000"), length.out = n_size)
shapes <- rep(c(21, 24, 22, 23, 25), length.out = n_size)
pd     <- ggplot2::position_dodge(width = 0.72)

## ---- 5. The figure ---------------------------------------------------------
p <- ggplot2::ggplot(plot_df,
                     ggplot2::aes(x = Test, y = Statistical_Power,
                                  colour = DataSize, shape = DataSize, fill = DataSize)) +
  ggplot2::geom_blank() +
  ggplot2::geom_hline(yintercept = target_pow, linetype = "dashed",
                      linewidth = 0.6, colour = "black") +
  ggplot2::geom_vline(xintercept = seq(1.5, nlevels(plot_df$Test) - 0.5, 1),
                      colour = "grey80", linewidth = 0.3) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = Lower_CI, ymax = Upper_CI),
                         position = pd, width = 0, linewidth = 0.6) +
  ggplot2::geom_point(position = pd, size = 2.4, stroke = 0.45,
                      colour = "black") +
  ggplot2::facet_wrap(~ Panel, ncol = 1) +
  ggplot2::scale_colour_manual(values = cols) +
  ggplot2::scale_fill_manual(values = cols) +
  ggplot2::scale_shape_manual(values = shapes) +
  ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                              labels = scales::label_number(accuracy = 0.1),
                              expand = ggplot2::expansion(mult = c(0.03, 0.03))) +
  ggplot2::labs(x = "Test", y = "Empirical power",
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
    strip.background   = ggplot2::element_rect(fill = "grey90", colour = "black",
                                               linewidth = 0.5),
    strip.text         = ggplot2::element_text(face = "bold", size = 11.5,
                                               margin = ggplot2::margin(4, 0, 4, 0)),
    panel.spacing      = ggplot2::unit(0.9, "lines"),
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
pdf_file <- file.path(out_dir, "Power_plot.pdf")
png_file <- file.path(out_dir, "Power_plot.png")

ok_pdf <- tryCatch({
  ggplot2::ggsave(pdf_file, p, width = fig_width, height = fig_height,
                  device = grDevices::cairo_pdf); TRUE
}, error = function(e) FALSE)
if (!ok_pdf)
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
# ggplot2::ggsave(file.path(out_dir, "Power_plot.tiff"), p,
#                 width = fig_width, height = fig_height, dpi = fig_dpi,
#                 device = "tiff", compression = "lzw", bg = "white")