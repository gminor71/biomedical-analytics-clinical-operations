# utils_visuals.R
# Project-wide visual standards for figures

COLORS <- list(
  control = "grey30",
  active  = "#0072B2",   # Okabe-Ito blue (colorblind safe)
  ci_band = "grey70",
  ref_line = "grey50"
)

THEME_CLINICAL <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

save_png <- function(path, plot_obj, width = 7.5, height = 5.2, dpi = 150) {
  ggplot2::ggsave(
    filename = path,
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
}

save_png_base <- function(path, expr, width = 900, height = 650, res = 120) {
  grDevices::png(
    filename = path,
    width = width,
    height = height,
    res = res,
    bg = "white",
    type = "cairo-png"
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
}

