## ---- theme-remark
theme_remark <- function() {
  theme_grey() +
    theme(
      axis.text = element_text(size = 24),
      strip.text = element_text(size = 24,
                                margin = margin()),
      axis.title = element_text(size = 24),
      legend.title = element_text(size = 24),
      legend.text = element_text(size = 24),
      legend.position = "bottom",
      plot.title =  element_text(size = 24)
    ) 
}

## ---- theme-alldist
theme_alldist <- function() {
  theme_grey() +
    theme(
      axis.text = element_text(size = 16),
      strip.text = element_text(size = 16, margin = margin()),
      axis.title = element_text(size = 16),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 16),
      plot.title =  element_text(size = 16)
    )
}
