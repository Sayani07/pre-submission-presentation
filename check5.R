library(tidyverse)
library(ggplot2)
library(gravitas)

id1_tsibble1 <- id1_tsibble %>% 
  create_gran("week_month") %>% 
  create_gran("wknd_wday") %>% 
  as_tibble() %>% 
  select(week_month, wknd_wday, kwh)

#prob <- seq(0.01,0.99,by=0.1)
#prob <- c(0.01,0.1, 0.25, 0.5, 0.75, 0.9, 0.99)
prob <- c(0.25, 0.5, 0.75)

id1_tsibble1 %>% 
  group_by(week_month, wknd_wday) %>% 
  summarise()

nhour = unique(id1_tsibble1$week_month) 
nwday = unique(id1_tsibble1$wknd_wday) 

percetile_data <- lapply(seq_len(length(nhour)),
                         function(x){
                           lapply(seq_len(length(nwday)), function(y){
                             data <- id1_tsibble1 %>% filter(week_month==nhour[x], wknd_wday==nwday[y])
                             quantile(data$kwh, prob = prob, type = 8)
                           }) %>% bind_rows(.id = "x")
                         }) %>% bind_rows(.id = "wom")


all_data <- percetile_data %>% 
  pivot_longer(-c(1:2),
               values_to = "values", names_to= "percentiles")

all_data$wom = as_factor(all_data$wom)
all_data$x = as_factor(all_data$x)

p <- all_data %>% 
  filter(wom != 5) %>% 
  ggplot(aes(x = x, y = values, colour = percentiles, group = percentiles))+
  geom_line(size = 1) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        strip.text = element_text(size = 10, margin = margin(b = 0, t = 0)),
        panel.grid.major = element_blank()) +
  scale_color_viridis_d(direction = -1) +
  facet_wrap(~wom, labeller = "label_both") +  xlab("") +
  scale_x_discrete(labels = c("weekday", "weekend")) +
  ylab("")


ggsave("plot4.png", p, "png", path = "./figs/", dpi= 300, height = 19, unit = "cm")


