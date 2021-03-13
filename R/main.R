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
library(lubridate)
library(patchwork)
library(here)
library(parallel)
theme_set(theme_bw())
library(readr)

## ---- load-elecnew


elec <- read_rds(here("data/sm_cust_data.rds"))


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


## ---- elec-rawnew

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

##----rank-table
knitr::include_graphics("figs/rank-table.png")
#read_rds("data/elec_rank.rds")

##----validate-household1
knitr::include_graphics("figs/validate-household1.png")

##----algorithm1
knitr::include_graphics("figs/algorithm1.png")

##----example-design2

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

## ----permutation-test

theme_permutation <- function(){
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
  #geom_point(size = 2, alpha = 0.7) +
  theme_classic() +
    theme(panel.spacing =unit(0, "lines"),
          panel.border = element_rect(color = "black", fill = NA, size = 1), 
          strip.background = element_rect(color = "black", size = 1),
          legend.position = "none",
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.title = element_text(hjust = 0.5)) 
}

set.seed(123)
sim_panel_null <- sim_panel(
  nx = 2,
  nfacet = 3,
  ntimes = 10,
  sim_dist = distributional
  ::dist_normal(5, 10)
) %>% unnest(c(data)) %>% 
  mutate(colour = paste(id_facet, id_x, sep = "-"))

sim_obs <- sim_panel_null %>% 
  ggplot(aes(x = as.factor(id_x),
             y = sim_data, color = as.factor(colour))) + 
           facet_wrap(~id_facet) +
  geom_point(size = 2, alpha = 0.7) + 
  xlab("") +
  ylab("") +
  #scale_color_manual(values = c("#0072B2", "#D55E00")) +
  scale_color_viridis_d() +
  theme_remark() +
      theme_permutation() +
  ggtitle("original")

  
sim_panel_null$colour <- sample(sim_panel_null$colour)

sim_perm <- sim_panel_null %>% 
  ggplot(aes(x = as.factor(id_x),
             y = sim_data, color = colour)) + 
  facet_wrap(~id_facet) +
  geom_point(size = 2, alpha = 0.7) + 
  xlab("") +
  ylab("") +
  #scale_color_manual(values = c("#0072B2", "#D55E00")) +
  scale_color_viridis_d() +
theme_remark() +
  theme_permutation() +
  ggtitle("perm id: 1")

sim_panel_null$colour <- sample(sim_panel_null$colour)

sim_perm1 <- sim_panel_null %>% 
  ggplot(aes(x = as.factor(id_x),
             y = sim_data, color = colour)) + 
  facet_wrap(~id_facet) +
  geom_point(size = 2, alpha = 0.7) + 
  xlab("") +
  ylab("") +
  #scale_color_manual(values = c("#0072B2", "#D55E00")) +
  scale_color_viridis_d() +
  theme_remark() +
  theme_permutation()+
  ggtitle("perm id: 2")
  

sim_panel_null$colour <- sample(sim_panel_null$colour)

sim_perm2 <- sim_panel_null %>% 
  ggplot(aes(x = as.factor(id_x),
             y = sim_data, color = colour)) + 
  facet_wrap(~id_facet) +
  geom_point(size = 2, alpha = 0.7) + 
  xlab("") +
  ylab("") +
  #scale_color_manual(values = c("#0072B2", "#D55E00")) +
  scale_color_viridis_d() +
  theme_remark() +
  theme_permutation()+
  ggtitle("perm id: 199")


sim_ob2 <- sim_obs +
  #scale_color_manual(values = c("#D55E00", "#0072B2")) +
  scale_color_viridis_d(direction = -1) +
  theme_remark() +
  theme_permutation()+
  ggtitle("perm id: 200")

sim_final <- sim_obs  + (sim_perm + sim_perm1)/ (sim_perm2 + sim_ob2)

sim_final


##----raw-dist

G21 <- read_rds("simulations/raw/all_data_wpd_Gamma21.rds")

G21_dist <- G21 %>% 
  ggplot(aes(x = value)) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  scale_x_continuous(breaks = scales::breaks_extended(3)) + 
  xlab("wpd")

G21_rel <- G21 %>% 
  ggplot(aes(x=nx*nfacet, y = value)) +
  geom_point(alpha = 0.5, size = 0.5) + stat_summary(fun=median, geom="line", aes(group=1), color = "blue") + xlab("nx*nfacet") + ylab("wpd") + theme_remark()

library(patchwork)
#G21_rel +
  G21_dist 
#+ plot_layout(widths = c(1,2))
##----perm-dist
G21_norm <- read_rds("simulations/norm/all_data_wpd_Gamma21.rds")

G21_norm %>% 
  ggplot(aes(x = value)) + 
  geom_density(fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") + 
  scale_x_continuous(breaks = scales::breaks_extended(3)) + 
  xlab("wpd normalised using permutation approach")

##----glm2
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


G21_residual <- G21 %>% 
  ggplot(aes(x=log(nx*nfacet),
             y = (value - (1/(intercept + slope*log(nx*nfacet)
             )
             )
             )
  )
  ) +
  geom_point(alpha = 1, size = 1) + stat_summary(fun=mean, geom="line", aes(group=1), color = "blue") + 
  ylab("residuals") + ggtitle("Residual plot")


G21_glm <- G21 %>% 
  mutate(wpd_glm =  (value - (1/(intercept + slope*log(nx*nfacet)
  )
  )
  ),
  wpd_glm_scaled = ((wpd_glm*320)))


G21_dist <- G21_glm %>% 
  ggplot() +
  geom_density(aes(x = wpd_glm), 
               fill = "blue") +
  facet_grid(nx~nfacet,
             labeller = "label_both") +
  theme(legend.position = "bottom") 


G21_residual + 
  theme(
    axis.text = element_text(size = 32),
    strip.text = element_text(size = 32,
                              margin = margin()),
    axis.title = element_text(size = 32),
    legend.title = element_text(size = 32),
    legend.text = element_text(size = 32),
    legend.position = "bottom",
    plot.title =  element_text(size = 32)
  ) + 
  theme(plot.title = element_text(hjust = 0.5))

##----glm-dist
G21_dist
##----allplots

knitr::include_graphics("figs/allplots.png")

##----graphical map

knitr::include_graphics("figs/graphical_map.png")

##----clash

VIC <- tsibbledata::vic_elec

clash <- VIC %>%
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

noclash <- VIC %>%
  create_gran("month_year") %>%
  filter(month_year %in% c("Jan", "Mar", "July", "Dec")) %>%
  prob_plot("month_year", "day_week",
            response = "Demand",
            plot_type = "lv") + ggtitle("") + theme_remark() +
  theme(
    axis.text = element_text(size = 16)) + 
  scale_x_discrete(breaks = c("Sun", "Wed", "Fri"))+  ylab("Energy consumption (kwh)")

##----bothclash2
clash + noclash

##----same-scale

G21_permutation <- read_rds("simulations/norm/all_data_wpd_N01.rds") %>%
  rename("wpd_permutation" = "value")


# G21_model_data <- G21 %>%
#   mutate(model =
#            ((1/value)
#                   - intercept -
#                     slope*log(nx*nfacet))/slope) %>%
#   mutate(model_trans =
#            (model - mean(model))/sd(model))

# G21_model_data$model %>% summary()

G21_all_data <- G21_permutation %>% 
  # left_join(G21_lm, by = c("nx", "nfacet", "perm_id")) %>% 
  left_join(G21_glm, by = c("nx", "nfacet", "perm_id")) %>% 
  pivot_longer(cols = c(3, 7),
               names_to = "type_estimate",
               values_to = "value_estimate")
G21_all_data$type_estimate = factor(G21_all_data$type_estimate , levels = c( "wpd_permutation", "wpd_glm_scaled"))


G21_all_data %>% 
  filter(type_estimate %in% c("wpd_glm_scaled", "wpd_permutation")) %>% 
  ggplot() +
  geom_density(aes(x = value_estimate, 
                   fill = type_estimate), alpha = 0.5) +
  facet_grid(nx~nfacet,
             labeller = "label_both") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c( "#D55E00", "#0072B2")) +
  xlab("wpd_norm2") +
  scale_x_continuous(breaks = c(-5, -3, 0, 3, 5))

##----linear-scale-8new

library(scales)
library(tidyquant)
library(gghighlight)
library(lubridate)
library(here)
library(tidyverse)
library(gghighlight)

elec <- read_rds(here("data/elec_all-8.rds")) %>% 
  dplyr::filter(date(reading_datetime) >= ymd("20190701"), date(reading_datetime) < ymd("20191231"), meter_id==1) %>% 
  select(-meter_id) %>% 
  rename("id" = "household_id",
         "date_time" = "reading_datetime") %>% 
  mutate(date = date(date_time))


elec_linear <- elec %>% 
  ggplot() +
  geom_line(aes(x = date_time, y = kwh),alpha = 0.7) +
  facet_wrap(~id, nrow = 8, labeller = "label_both",
             strip.position =  "right",
             scales = "free_y") + ggtitle("Jun-19 to Dec-19 demand") + ylab("Demand (kwh)")

elec_zoom <-  elec %>%
  as_tibble() %>% 
  filter(date >as.Date("2019-09-01") & date < (as.Date("2019-09-30"))) %>% 
  ggplot(aes(x=date_time, y = kwh)) +
  geom_line(size = 0.1, colour = "blue") +
  facet_wrap(~id, 
             scales = "free_y",
             ncol = 1,
             strip.position =  "right") +
  gghighlight(date > as.Date("2019-09-15") & date < (as.Date("2019-09-21")), unhighlighted_params = list(colour = "black")) + ggtitle("Zoomed in for Sep-19") + ylab("Demand (kwh)")

p <- elec_linear + elec_zoom

p

##----search-gran

elec %>% 
  search_gran(lowest_unit = "hour",
              highest_unit = "month", 
              filter_in = "wknd_wday", 
              filter_out = "fortnight") 

##---- harmony
elec %>% harmony(ugran = "month",
                 filter_in = "wknd_wday",
                 filter_out = c("hhour", "fortnight")
  )

##---- select-harmonies
elec_select_harmony = parallel::mclapply(1:8, function(x){
  
  data_id <-  elec_split %>% magrittr::extract2(x) %>% 
    as_tsibble(index = date_time)
  
  harmonies <- data_id %>%
    harmony(
      ugran = "month",
      filter_in = "wknd_wday",
      filter_out = c("hhour", "fortnight")
    )
  
  hakear::select_harmonies(data_id,
                           harmony_tbl = harmonies,
                           response = kwh,
                           nperm = 200,
                           nsamp = 200
  )
  
}, mc.cores = parallel::detectCores() - 1, mc.preschedule = FALSE, mc.set.seed = FALSE)
#toc()


elec_harmony_all <- elec_select_harmony %>% 
  bind_rows(.id = "id") %>% 
  mutate(facet_variable = case_when(
    facet_variable == "hour_day" ~ "hod" ,
    facet_variable == "day_month" ~ "dom" ,
    facet_variable == "day_week" ~ "dow" ,
    facet_variable == "week_month" ~ "wom" ,
    facet_variable == "wknd_wday" ~ "wdwnd"
  )) %>% 
  mutate(x_variable = case_when(
    x_variable == "hour_day" ~ "hod" ,
    x_variable == "day_month" ~ "dom" ,
    x_variable == "day_week" ~ "dow" ,
    x_variable == "week_month" ~ "wom" ,
    x_variable == "wknd_wday" ~ "wdwnd"
  )) %>% 
  mutate(id = paste("id", id, sep = " ")) %>% 
  group_by(id) %>% 
  mutate(rank = row_number())

select_split <- str_split(elec_harmony_all$select_harmony, " ", simplify = TRUE)[,2]

elec_sig_split <- elec_harmony_all %>% 
  bind_cols(select_split = select_split) %>% 
  mutate(significant = case_when(
    select_split == "***" ~ "highest",
    select_split == "**" ~ "high",
    select_split == "*" ~ "medium",
    select_split == "" ~ "low"
  )) %>% 
  mutate(rank = case_when(
    select_split == "***" ~ paste(rank, "***", sep = " "),
    select_split == "**" ~  paste(rank, "**", sep = " "),
    select_split == "*" ~  paste(rank, "*", sep = " "),
    select_split == "" ~  paste(rank, "", sep = " ")
  ))

elec_rank <- elec_sig_split %>% 
  select(-c(6,7, 9, 10)) %>% 
  pivot_wider(
    names_from = id,
    values_from = rank) %>% 
  rename("facet variable" = "facet_variable",
         "x variable" = "x_variable") %>% 
  select(-facet_levels, -x_levels)

##----heatplot-new


elec_sig_split <- read_rds("data/elec_sig_split.rds")
  
heatplot <- elec_sig_split %>% 
  mutate(significance_95 = if_else(significant %in% c("high", "highest"), "*", "")) %>% 
  ggplot(aes(x = x_variable,
             y = facet_variable)) +
  geom_tile(aes(fill = wpd)) + 
  #color = as.factor(significance_95)),
  #size = 0.3) +
  geom_text(aes(label = significance_95), color = "#42275a") +
  scale_fill_gradient(low = "white",high = "#ba5370") +
  #scale_fill_manual(palette = "Dark2") +
  #scale_colour_manual(values = c("white","red")) + 
  theme(legend.position = "bottom") +
  coord_fixed() + 
  guides(fill = guide_legend()) +
  theme_void() +
  theme_gray(base_size = 12, base_family = "Times") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 60, hjust=1),legend.position = "bottom") + ggtitle("") +
  theme(
    strip.background = element_blank(),
    strip.text.y  = element_blank(), plot.margin = unit(c(0, -2, 0, 0), "cm"))

elec <- read_rds(here("data/elec_all-8.rds")) %>% 
  dplyr::filter(date(reading_datetime) >= ymd("20190701"), date(reading_datetime) < ymd("20191231"), meter_id==1) %>% 
  select(-meter_id) %>% 
  rename("id" = "household_id",
         "date_time" = "reading_datetime")

elec_zoom <-  elec %>%
  as_tibble() %>% 
  dplyr::filter(date(date_time) > as.Date("2019-09-01") & date(date_time) < (as.Date("2019-09-30"))) %>%
  ggplot(aes(x=date_time, y = kwh)) +
  #geom_point(size = 0.1, colour = "black", alpha = 0.3) +
  geom_line(size = 0.1, colour = "blue") +
  facet_wrap(~id, 
             scales = "free_y",
             ncol = 1,
             strip.position =  "right",
             labeller = "label_both") + 
  xlab("Time [30m]") + 
  theme_grey() + 
  ylab("") + ggtitle("")

p <- heatplot +  facet_grid(id~.) + elec_zoom +
  theme( plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  plot_layout(widths = c(1, 2)) 

p

# ggsave("heatplot.png", p, "png", path = "./figs/", dpi= 700, height = 25, unit = "cm")
# 
# ggsave("eleclinear8.tiff", p, units = "in",  path = "./figs/", width = 5,  device = "tiff", dpi = 700)

# ggsave("heatplot.png", p, "png", path = "./figs/", dpi= 300, height = 25, unit = "cm")

#knitr::include_graphics("figs/eleclinear8.tiff")

##----heatplot-call2
ggsave("heatplot.png", p, "png", path = "./figs/", dpi= 300, height = 20, unit = "cm")
knitr::include_graphics("figs/heatplot.png")

##----gravitas-plot
id1_tsibble <- elec %>%
  filter(id== 1)

p1 <- id1_tsibble %>% 
  prob_plot("hour_day",
            "wknd_wday",
            response = "kwh",
            plot_type = "quantile",
            symmetric = TRUE,
            quantile_prob = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99)) +
  ggtitle("a) hod vs wdwnd (Rank 1)") + 
  scale_colour_brewer(name = "", palette = "Set2") +
  theme(legend.position = "none",
        strip.text = element_text(size = 7, margin = margin()),)


p2 <- id1_tsibble %>%
  prob_plot("wknd_wday",
            "hour_day",
            response = "kwh",
            plot_type = "quantile",
            symmetric = FALSE,
            quantile_prob = c(0.25,0.5,0.75)) +
  ggtitle("c) wdwnd vs hod (Rank 3)") +
  scale_colour_brewer(name = "", palette = "Set2")   +
  theme(legend.position = "none",
        strip.text = element_text(size = 7,
                                  margin = margin()))  +
  scale_x_discrete(breaks = seq(0, 23, 5))

p3 <- id1_tsibble %>%
  prob_plot("week_month",
            "wknd_wday",
            response = "kwh",
            plot_type = "quantile",
            symmetric = FALSE,
            quantile_prob = 
              c(0.25,0.5,0.75)) +
  scale_colour_brewer(name = "", palette = "Set2") +
  theme(
    strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) + theme(legend.position = "bottom",
                                                                                 strip.text = element_text(size = 7, margin = margin())) +
  ggtitle("d) wom vs wdwnd (insignificant)")

p4 <- id1_tsibble %>%
  prob_plot("day_month",
            "hour_day",
            response = "kwh",
            plot_type = "quantile",
            symmetric = FALSE,
            quantile_prob = 
              c(0.25,0.5,0.75)) +
  scale_colour_brewer(name = "", palette = "Set2") +
  theme(
    strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) + theme(legend.position = "bottom",
                                                                                 strip.text = element_text(size = 7, margin = margin())) +
  ggtitle("b) dom vs hod (Rank 2)") +
  scale_x_discrete(breaks = seq(0, 31, 5))

(p1 + p4)/(p2 + p3) + theme_classic() 
  

## ---- theme
knitr::include_graphics("figs/theme.png")

## ---- motivation
knitr::include_graphics("figs/motivation.png")
