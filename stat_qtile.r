

########################################################################
#
# A stat

StatQtile <- ggproto(
  "StatQtile", Stat, required_aes = c("x", "y"),
  
  compute_group = function(data, scales, upper, lower) {
    data %>% 
      group_by(x) %>%
      summarise(ymax=quantile(y, upper),
                ymin=quantile(y, lower),
                y=median(y))
  }
)

stat_qtile <- function(mapping = NULL, data = NULL, geom = "smooth",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, upper=0.95, lower=0.05, ...) {
  layer(
    stat = StatQtile, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, upper=upper, lower=lower, se=TRUE, ...)
  )
}
