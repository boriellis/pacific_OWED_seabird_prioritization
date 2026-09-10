##########################################################################
# Pacific Seabird OWED Prioritization Framework #########################
# SCRATCH — 3D vulnerability scatter (candidate for future Shiny app use)
##########################################################################
#
# Interactive plotly version of the exposure x sensitivity x threat scatter,
# with vulnerability score as color. Not used in the manuscript; retained as
# a starting point in case the Shiny app is extended to an interactive 3D
# view. See paper Figure 4 (make_vuln_scatter in R/visualizations.R) for the
# static 2D version actually used.
#-------------------------------------------------------------------------

library(plotly)
library(scico)

# assumes priority321 is loaded (see scripts/05_plot.R, Part 1)

d <- priority321 %>%
  filter(region == "CA", !is.na(ess)) %>%
  mutate(x = log2(e),      # weighted, so planes are 45°
         y = log2(sensitivity^2),
         z = log2(status))

# grid for the iso-vulnerability planes: x + y + z = log2(V)
gx <- seq(min(d$x), max(d$x), length.out = 30)
gy <- seq(min(d$y), max(d$y), length.out = 30)
plane <- function(V) {
  z <- outer(gx, gy, \(a, b) log2(V) - a - b)
  z[z < -1 | z > 1] <- NA        # drop anything outside the threat range
  z
}

pal <- scico::scico(256, palette = "lipari")
colorscale <- lapply(seq_along(pal), \(i) list((i - 1) / (length(pal) - 1), pal[i]))

p <- plot_ly() %>%
  add_markers(
    data = d, x = ~x, y = ~y, z = ~z,
    text = ~common_name,
    marker = list(
      size = 4,
      color = ~log2(ess),
      colorscale = colorscale,
      cmin = -6, cmax = 6,
      colorbar = list(
        title = "Vulnerability",
        tickvals = seq(-6, 6, 2),
        ticktext = c("0.0156", "0.0625", "0.25", "1", "4", "16", "64")
      )
    )
  ) %>%
  layout(scene = list(
    xaxis = list(title = "log2(Exposure\u00b3)",    range = c(-3, 3), autorange = FALSE),
    yaxis = list(title = "log2(Sensitivity\u00b2)", range = c(-2, 2), autorange = FALSE),
    zaxis = list(title = "log2(Threat)",       range = c(-1, 1), autorange = FALSE),
    camera = list(eye = list(x = 1.6, y = 1.6, z = 1.0)),
    aspectmode = "manual",
    aspectratio = list(x = 3, y = 2, z = 1)
  ))
p