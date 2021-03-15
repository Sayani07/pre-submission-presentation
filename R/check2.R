library(tidyverse)
library(ggplot2)
library(gravitas)

id1_tsibble1 <- id1_tsibble %>% 
  create_gran("day_month") %>% 
  create_gran("hour_day") %>% 
  as_tibble() %>% 
  select(day_month, hour_day, kwh)

#prob <- seq(0.01,0.99,by=0.1)
#prob <- c(0.01,0.1, 0.25, 0.5, 0.75, 0.9, 0.99)
prob <- c(0.25, 0.5, 0.75)

id1_tsibble1 %>% 
  group_by(day_month, hour_day) %>% 
  summarise()

nhour = unique(id1_tsibble1$day_month) 
nwday = unique(id1_tsibble1$hour_day) 

percetile_data <- lapply(seq_len(length(nhour)),
                         function(x){
                           lapply(seq_len(length(nwday)), function(y){
                             data <- id1_tsibble1 %>% filter(day_month==nhour[x], hour_day==nwday[y])
                             quantile(data$kwh, prob = prob, type = 8)
                           }) %>% bind_rows(.id = "x")
                         }) %>% bind_rows(.id = "dom")


all_data <- percetile_data %>% 
  pivot_longer(-c(1:2),
               values_to = "values", names_to= "percentiles")

all_data$dom = as_factor(all_data$dom)
all_data$x = as_factor(all_data$x)

p <- all_data %>% 
  filter(dom!=31) %>% 
  ggplot(aes(x = x, y = values, colour = percentiles, group = percentiles))+
  geom_line(size = 1) +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        strip.text = element_text(size = 10, margin = margin(b = 0, t = 0)),
        panel.grid.major = element_blank()) +
  scale_color_viridis_d(direction = -1) +
  facet_wrap(~dom, labeller = "label_both") + 
  scale_x_discrete(breaks = seq(0, 31, 5)) +
  xlab("hour_day") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) 

ggsave("plot2.png", p, "png", path = "./figs/", dpi= 300, height = 19, unit = "cm")

