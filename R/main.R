## ---- load
source("R/theme.R")
library(tidyverse)
library(sugrrants)
library(tsibble)
library(gravitas)
library(kableExtra)
library(gganimate)
library(lubridate)
library(ggridges)
library(ggpubr)
library(lvplot)
library(kableExtra)
library(gghdr)
library(tsibbledata)
library(rvest)
library(htmltools)
library(transformr)

## ---- load-elec
library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
library(patchwork)
library(here)
theme_set(theme_bw())
library(readr)

elec <- read_rds(here("../paper-hakear/paper/data/sm_cust_data.rds"))


elec_ts <- elec %>% 
  build_tsibble(
    key = customer_id, index = reading_datetime,
    validate = FALSE, ordered = TRUE
  )

## ---- elec-gaps
gap_df <- has_gaps(elec_ts)
# sum(gap_df$.gaps) / NROW(gap_df)

## ---- count-gaps
count_na_df <- elec_ts %>% 
  count_gaps()

lumped_na_df <- count_na_df %>% 
  mutate(
    customer_id = as.factor(customer_id) %>% 
      # fct_lump(50) %>%
      fct_reorder(.n, sum)
  ) 

# p_49 <- lumped_na_df %>% 
#   filter(customer_id != "Other") %>% 
#   ggplot(aes(x = customer_id)) +
#   geom_linerange(aes(ymin = .from, ymax = .to)) +
#   geom_point(aes(y = .from), size = 0.6, shape = 4) +
#   geom_point(aes(y = .to), size = 0.6, shape = 4) +
#   coord_flip() +
#   xlab("50 top customers") +
#   ylab("") +
#   theme(
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     legend.position = "none",
#     plot.margin = unit(c(0, 0, -1, -1), "line")
#   )

p_other <- lumped_na_df %>% 
  # filter(customer_id == "Other") %>% 
  ggplot(aes(x = customer_id)) +
  geom_linerange(aes(ymin = .from, ymax = .to), alpha = 0.2) +
  geom_point(aes(y = .from), size = 1.2, shape = 4, alpha = 0.2) +
  geom_point(aes(y = .to), size = 1.2, shape = 4, alpha = 0.2) +
  coord_flip() +
  xlab("100 sampled customers") +
  ylab("Time gaps") +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )

p_other + plot_layout(ncol = 1, heights = c(10, 1))


## ---- elec-raw

sm_50 <- elec %>% 
  distinct(customer_id) %>% 
  slice(1:50)

elec %>%
  filter(customer_id %in% sm_50$customer_id) %>% 
  tibble() %>% 
  ggplot(aes(x=reading_datetime, 
             y= general_supply_kwh), alpha = 0.5) + 
  geom_line() +
  scale_fill_gradient2() +
  facet_wrap(~customer_id, ncol= 2) +
  theme_void() +
  theme(strip.text.x = element_blank()) +
  xlab("Time [30m]") +
  ylab("electricity demand in kwh")


## ---- elec-cyclic
sm_50 <- elec %>% 
  distinct(customer_id) %>% 
  slice(1:50)

elec %>%
  filter(customer_id %in% sm_50$customer_id) %>% 
  tibble() %>% 
  ggplot(aes(x=hour(reading_datetime), 
             y= general_supply_kwh), alpha = 0.5) + 
  geom_point() +
  scale_fill_gradient2() +
  facet_wrap(~customer_id, ncol= 2) +
  theme_void() +
  theme(strip.text.x = element_blank()) +
  xlab("Time [30m]") +
  ylab("electricity demand in kwh")


##----dist-explain
knitr::include_graphics("figs/dist_explain.png")

##----heatmap
knitr::include_graphics("figs/heatmap-8.png")

##----rank-table0
knitr::include_graphics("figs/rank-table.png")

##----validate-household1
knitr::include_graphics("figs/validate-household1.png")

##----
knitr::include_graphics("figs/algorithm1.png")