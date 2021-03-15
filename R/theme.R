## ---- theme-remark
theme_remark <- function() {
  theme_grey() +
    theme(
      axis.text = element_text(size = 18),
      strip.text = element_text(size = 18,
                                margin = margin()),
      axis.title = element_text(size = 18),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.position = "bottom",
      plot.title =  element_text(size = 18)
    ) 
}

## ---- theme-alldist
theme_alldist <- function() {
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
    ) + 
    theme(
      axis.text = element_text(size = 16),
      strip.text = element_text(size = 16, margin = margin()),
      axis.title = element_text(size = 16),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 16),
      plot.title =  element_text(size = 16)
    )
}
## ---- theme-permutation
theme_permutation <- function(){
  #geom_point(size = 2, alpha = 0.7) +
  theme_classic() +
  theme(panel.spacing =unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1),
        legend.position = "none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 
}

theme_fonts <- function(){
  theme(
    axis.text = element_text(size = 18),
    strip.text = element_text(size = 20,
                              margin = margin()),
    axis.title = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.position = "bottom",
    plot.title =  element_text(size = 18)
  ) 
}
