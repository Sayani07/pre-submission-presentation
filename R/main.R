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
library(hakear)
library(distributional)
library(readr)

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


##----linear2cyclic

load("data/sm_cust50.Rdata")


sm_cust50 <- sm_cust50 %>% 
  tsibble::as_tsibble(regular = FALSE)

smart_meter50 <-  sm_cust50 %>%
  select(customer_id, 
         reading_datetime,
         general_supply_kwh, 
         everything())

data_cust1 <- smart_meter50 %>% filter(customer_id == 10017936)

data_cust1%>% ggplot() + geom_line(aes(x =reading_datetime, y = general_supply_kwh), color = "#1B9E77")+ theme(legend.position = "bottom") + ylab("")+ 
  theme_remark()


smart_meter50 %>%
  filter(customer_id==10018250) %>%
  mutate(hour_day = hour(reading_datetime)) %>% 
  ggplot() + geom_point(aes(x = hour_day, 
                            y = general_supply_kwh), color = "#1B9E77") + 
  theme_remark() + ylab("")




##----dist-explain
knitr::include_graphics("figs/dist_explain.png")

##----heatmap
knitr::include_graphics("figs/heatmap-8.png")

##----rank-table0
knitr::include_graphics("figs/rank-table.png")

##----validate-household1
knitr::include_graphics("figs/validate-household1.png")

##----algorithm1
knitr::include_graphics("figs/algorithm1.png")

##----example-design

sim_varall_normal <- function(nx, nfacet, mean, sd, w) {
  dist_normal((mean + seq(0,
                          (nx *
                             nfacet - 1),
                          by = 1
  ) * w), sd)
}
sim_panel_varall <- sim_panel(
  nx = 2, nfacet = 3,
  ntimes = 500,
  sim_dist = sim_varall_normal(2, 3, 5, 10, 5)
) %>% unnest(data)

p_varall <- sim_panel_varall %>%
  rename("facet level" = "id_facet" ) %>% 
  ggplot(aes(x = as.factor(id_x), y = sim_data)) + 
  facet_wrap(~`facet level`,labeller = "label_both") + 
  geom_boxplot() +
  ggtitle("") +
  xlab("x level") +
  ylab("")


sim_varx_normal <- function(nx, nfacet, mean, sd, w) {
  rep(dist_normal((mean + seq(0, nx - 1, by = 1) * w), sd), nfacet)
}

sim_panel_varx <- sim_panel(
  nx = 2, nfacet = 3,
  ntimes = 500,
  sim_dist = sim_varx_normal(2, 3, 5, 10, 5)
) %>% unnest(data)


p_varx <- sim_panel_varx %>%
  rename("facet level" = "id_facet" ) %>% 
  ggplot(aes(x = as.factor(id_x), y = sim_data)) + 
  facet_wrap(~`facet level`,labeller = "label_both") + 
  ggtitle("") +
  geom_boxplot() +
  xlab("x level") +
  ylab("simulated response")



sim_varf_normal <- function(nx, nfacet, mean, sd, w) {
  rep(dist_normal((mean + seq(0, nfacet - 1, by = 1) * w), sd), each = nx)
}

sim_panel_varf <- sim_panel(
  nx = 2, nfacet = 3,
  ntimes = 500,
  sim_dist = sim_varf_normal(2, 3, 5, 10, 5)
) %>% unnest(data)


p_varf <- sim_panel_varf %>%
  rename("facet level" = "id_facet" ) %>% 
  ggplot(aes(x = as.factor(id_x), y = sim_data)) +
  facet_wrap(~`facet level`,labeller = "label_both") + 
  geom_boxplot() +
  ggtitle("") +
  xlab("x level") +
  ylab("")



sim_panel_null <- sim_panel(
  nx = 2,
  nfacet = 3,
  ntimes = 500,
  sim_dist = distributional
  ::dist_normal(5, 10)
) %>% unnest(c(data))

set.seed(9999)


p_null <- sim_panel_null %>%
  rename("facet level" = "id_facet" ) %>% 
  ggplot(aes(x = as.factor(id_x), y = sim_data)) +
  facet_wrap(~`facet level`,labeller = "label_both") +
  geom_boxplot() +
  ggtitle("") +
  xlab("x level") +
  ylab("simulated response")


ggpubr::ggarrange(p_null, p_varf,  p_varx, p_varall, nrow = 2, ncol = 2,
                  common.legend = TRUE,
                  labels = c("a", "b", "c", "d"))

##----raw-dist

G21 <- read_rds("simulations/raw/all_data_wpd_Gamma21.rds")

G21 %>% 
  ggplot(aes(x = value)) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  scale_x_continuous(breaks = scales::breaks_extended(3)) + 
  xlab("wpd")

##----perm-dist
G21_norm <- read_rds("simulations/norm/all_data_wpd_Gamma21.rds")

G21_norm %>% 
  ggplot(aes(x = value)) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  scale_x_continuous(breaks = scales::breaks_extended(3)) + 
  xlab("wpd normalised using permutation approach")

##----glm
G21 <- read_rds("simulations/raw/all_data_wpd_N01.rds")

G21_median <- G21 %>% 
  group_by(nx*nfacet) %>% 
  summarise(actual = median(value))


glm_fit <- glm(actual ~ log(`nx * nfacet`),
               family = Gamma(link = "inverse"),
               data = G21_median)

intercept <- glm_fit$coefficients[1]
slope <- glm_fit$coefficients[2]


G21_sd  = G21 %>% 
  mutate(wpd_glm =  (value - (1/(intercept + slope*log(nx*nfacet)
  )
  )
  ))

scale_fac <- 1/G21_sd$wpd_glm %>% sd()


G21 %>% 
  ggplot(aes(x=log(nx*nfacet),
             y = (value - (1/(intercept + slope*log(nx*nfacet)
             )
             )
             )
  )
  ) +
  geom_point(alpha = 0.5, size = 0.5) + stat_summary(fun=mean, geom="line", aes(group=1), color = "blue") + 
  ylab("wpd_glm = wpd - 1/(a  + b*log(nx*nfacet))")


G21_glm <- G21 %>% 
  mutate(wpd_glm =  (value - (1/(intercept + slope*log(nx*nfacet)
  )
  )
  ),
  wpd_glm_scaled = ((wpd_glm*320)))


G21_glm %>% 
  ggplot() +
  geom_density(aes(x = wpd_glm), 
               fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") +
  theme(legend.position = "bottom") 


##----allplots

knitr::include_graphics("figs/allplots.png")


##----graphical map

knitr::include_graphics("figs/graphical_map.png")

##----clash

VIC <- tsibbledata::vic_elec

VIC %>%
  create_gran("month_year") %>%
  filter(month_year %in% c("Jan", "Jul", "Nov")) %>%
  prob_plot("month_year",
            "day_year",
            response = "Demand",
            plot_type = "quantile",
            quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9),
            symmetric = FALSE) + ggtitle("") + theme_remark() + 
  scale_x_discrete(breaks = seq(0, 364, 40)) +
  scale_color_brewer(palette = "Dark2") +
  ylab("Energy consumption (kwh)")

##----noclash

VIC <- tsibbledata::vic_elec

VIC %>%
  create_gran("month_year") %>%
  filter(month_year %in% c("Jan", "Mar", "July", "Dec")) %>%
  prob_plot("month_year", "day_week",
            response = "Demand",
            plot_type = "lv") + ggtitle("") + theme_remark() +
  theme(
    axis.text = element_text(size = 16)) + 
  scale_x_discrete(breaks = c("Sun", "Wed", "Fri"))+  ylab("Energy consumption (kwh)")