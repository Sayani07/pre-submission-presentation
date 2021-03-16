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
  ggplot(aes(
    x = reading_datetime,
    y = general_supply_kwh
  ), alpha = 0.5) +
  geom_line() +
  scale_fill_gradient2() +
  facet_wrap(~customer_id, ncol = 2) +
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
  ggplot(aes(
    x = hour(reading_datetime),
    y = general_supply_kwh
  ), alpha = 0.5) +
  geom_point() +
  scale_fill_gradient2() +
  facet_wrap(~customer_id, ncol = 2) +
  theme_void() +
  theme(strip.text.x = element_blank()) +
  xlab("Time [30m]") +
  ylab("electricity demand in kwh")


## ----linear2cyclic

load("data/sm_cust50.Rdata")


sm_cust50 <- sm_cust50 %>%
  tsibble::as_tsibble(regular = FALSE)

smart_meter50 <- sm_cust50 %>%
  select(
    customer_id,
    reading_datetime,
    general_supply_kwh,
    everything()
  )

data_cust1 <- smart_meter50 %>% filter(customer_id == 10017936)

data_cust1 %>% ggplot() +
  geom_line(aes(x = reading_datetime, y = general_supply_kwh), color = "#1B9E77") +
  theme(legend.position = "bottom") +
  ylab("") +
  theme_remark()


smart_meter50 %>%
  filter(customer_id == 10018250) %>%
  mutate(hour_day = hour(reading_datetime)) %>%
  ggplot() +
  geom_point(aes(
    x = hour_day,
    y = general_supply_kwh
  ), color = "#1B9E77") +
  theme_remark() +
  ylab("")




## ----dist-explain
knitr::include_graphics("figs/dist_explain.png")

## ----heatmap
knitr::include_graphics("figs/heatmap-8.png")

## ----rank-table
knitr::include_graphics("figs/rank-table2.png")
# read_rds("data/elec_rank.rds")

## ----validate-household1
knitr::include_graphics("figs/validate-household1.png")

## ----algorithm1
knitr::include_graphics("figs/algorithm_revise.png")

## ----example-design2

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
  rename("facet level" = "id_facet") %>%
  ggplot(aes(x = as.factor(id_x), y = sim_data)) +
  facet_wrap(~`facet level`, labeller = "label_both") +
  geom_boxplot(fatten = 2.5) +
  ggtitle("") +
  xlab("x level") +
  ylab("") +
  geom_jitter(alpha = 0.05, color = "blue") +
  theme(
    axis.text = element_text(size = 14),
    strip.text = element_text(
      size = 14,
      margin = margin()
    ),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 16)
  )



sim_varx_normal <- function(nx, nfacet, mean, sd, w) {
  rep(dist_normal((mean + seq(0, nx - 1, by = 1) * w), sd), nfacet)
}

sim_panel_varx <- sim_panel(
  nx = 2, nfacet = 3,
  ntimes = 500,
  sim_dist = sim_varx_normal(2, 3, 5, 10, 5)
) %>% unnest(data)


p_varx <- sim_panel_varx %>%
  rename("facet level" = "id_facet") %>%
  ggplot(aes(x = as.factor(id_x), y = sim_data)) +
  facet_wrap(~`facet level`, labeller = "label_both") +
  ggtitle("") +
  geom_boxplot(fatten = 2.5) +
  xlab("x level") +
  ylab("simulated response") +
  geom_jitter(alpha = 0.05, color = "blue") +
  theme(
    axis.text = element_text(size = 14),
    strip.text = element_text(
      size = 14,
      margin = margin()
    ),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 16)
  )





sim_varf_normal <- function(nx, nfacet, mean, sd, w) {
  rep(dist_normal((mean + seq(0, nfacet - 1, by = 1) * w), sd), each = nx)
}

sim_panel_varf <- sim_panel(
  nx = 2, nfacet = 3,
  ntimes = 500,
  sim_dist = sim_varf_normal(2, 3, 5, 10, 5)
) %>% unnest(data)


p_varf <- sim_panel_varf %>%
  rename("facet level" = "id_facet") %>%
  ggplot(aes(x = as.factor(id_x), y = sim_data)) +
  facet_wrap(~`facet level`, labeller = "label_both") +
  geom_boxplot(fatten = 2.5) +
  ggtitle("") +
  xlab("x level") +
  ylab("") +
  geom_jitter(alpha = 0.05, color = "blue") +
  theme(
    axis.text = element_text(size = 14),
    strip.text = element_text(
      size = 14,
      margin = margin()
    ),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 16)
  )





sim_panel_null <- sim_panel(
  nx = 2,
  nfacet = 3,
  ntimes = 500,
  sim_dist = distributional
  ::dist_normal(5, 10)
) %>% unnest(c(data))

set.seed(9999)


p_null <- sim_panel_null %>%
  rename("facet level" = "id_facet") %>%
  ggplot(aes(x = as.factor(id_x), y = sim_data)) +
  facet_wrap(~`facet level`, labeller = "label_both") +
  geom_boxplot(fatten = 2.5) +
  ggtitle("") +
  xlab("x level") +
  ylab("simulated response") +
  geom_jitter(alpha = 0.05, color = "blue") +
  theme(
    axis.text = element_text(size = 14),
    strip.text = element_text(
      size = 14,
      margin = margin()
    ),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 16)
  )




p_all <- ggpubr::ggarrange(p_null, p_varf, p_varx, p_varall,
  nrow = 2, ncol = 2,
  common.legend = TRUE,
  labels = c("a", "b", "c", "d")
)

ggsave("example-design.png", p_all, "png", path = "./figs/", dpi = 300, height = 19, unit = "cm")

knitr::include_graphics("figs/example-design.png")




## ----permutation-test

theme_permutation <- function() {
  theme(
    axis.text = element_text(size = 24),
    strip.text = element_text(
      size = 24,
      margin = margin()
    ),
    axis.title = element_text(size = 24),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 24),
    legend.position = "bottom",
    plot.title = element_text(size = 24)
  ) +
    # geom_point(size = 2, alpha = 0.7) +
    theme_classic() +
    theme(
      panel.spacing = unit(0, "lines"),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      strip.background = element_rect(color = "black", size = 1),
      legend.position = "none",
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
}

set.seed(123)
sim_panel_null <- sim_panel(
  nx = 2,
  nfacet = 3,
  ntimes = 10,
  sim_dist = distributional
  ::dist_normal(5, 10)
) %>%
  unnest(c(data)) %>%
  mutate(colour = paste(id_facet, id_x, sep = "-"))

sim_obs <- sim_panel_null %>%
  ggplot(aes(
    x = as.factor(id_x),
    y = sim_data, color = as.factor(colour)
  )) +
  facet_wrap(~id_facet) +
  geom_point(size = 2, alpha = 0.7) +
  xlab("") +
  ylab("") +
  # scale_color_manual(values = c("#0072B2", "#D55E00")) +
  scale_color_viridis_d() +
  theme_remark() +
  theme_permutation() +
  ggtitle("original")


sim_panel_null$colour <- sample(sim_panel_null$colour)

sim_perm <- sim_panel_null %>%
  ggplot(aes(
    x = as.factor(id_x),
    y = sim_data, color = colour
  )) +
  facet_wrap(~id_facet) +
  geom_point(size = 2, alpha = 0.7) +
  xlab("") +
  ylab("") +
  # scale_color_manual(values = c("#0072B2", "#D55E00")) +
  scale_color_viridis_d() +
  theme_remark() +
  theme_permutation() +
  ggtitle("perm id: 1")

sim_panel_null$colour <- sample(sim_panel_null$colour)

sim_perm1 <- sim_panel_null %>%
  ggplot(aes(
    x = as.factor(id_x),
    y = sim_data, color = colour
  )) +
  facet_wrap(~id_facet) +
  geom_point(size = 2, alpha = 0.7) +
  xlab("") +
  ylab("") +
  # scale_color_manual(values = c("#0072B2", "#D55E00")) +
  scale_color_viridis_d() +
  theme_remark() +
  theme_permutation() +
  ggtitle("perm id: 2")


sim_panel_null$colour <- sample(sim_panel_null$colour)

sim_perm2 <- sim_panel_null %>%
  ggplot(aes(
    x = as.factor(id_x),
    y = sim_data, color = colour
  )) +
  facet_wrap(~id_facet) +
  geom_point(size = 2, alpha = 0.7) +
  xlab("") +
  ylab("") +
  # scale_color_manual(values = c("#0072B2", "#D55E00")) +
  scale_color_viridis_d() +
  theme_remark() +
  theme_permutation() +
  ggtitle("perm id: 199")


sim_ob2 <- sim_obs +
  # scale_color_manual(values = c("#D55E00", "#0072B2")) +
  scale_color_viridis_d(direction = -1) +
  theme_remark() +
  theme_permutation() +
  ggtitle("perm id: 200")

sim_final <- sim_obs + (sim_perm + sim_perm1) / (sim_perm2 + sim_ob2)

sim_final


## ----raw-dist

G21 <- read_rds("simulations/raw/all_data_wpd_Gamma21.rds")

G21_dist <- G21 %>%
  ggplot(aes(x = value)) +
  geom_density(fill = "blue") +
  facet_grid(nx ~ nfacet,
    labeller = "label_both"
  ) +
  scale_x_continuous(breaks = scales::breaks_extended(3)) +
  xlab("wpd")

G21_rel <- G21 %>%
  ggplot(aes(x = nx * nfacet, y = value)) +
  geom_point(alpha = 0.5, size = 0.5) +
  stat_summary(fun = median, geom = "line", aes(group = 1), color = "blue") +
  xlab("nx*nfacet") +
  ylab("wpd") +
  theme_remark()

library(patchwork)
# G21_rel +
G21_dist
#+ plot_layout(widths = c(1,2))
## ----perm-dist
G21_norm <- read_rds("simulations/norm/all_data_wpd_Gamma21.rds")

G21_norm %>%
  ggplot(aes(x = value)) +
  geom_density(fill = "blue") +
  facet_grid(nx ~ nfacet,
    labeller = "label_both"
  ) +
  scale_x_continuous(breaks = scales::breaks_extended(3)) +
  xlab("wpd normalised using permutation approach")

## ----glm2
G21 <- read_rds("simulations/raw/all_data_wpd_N01.rds")

G21_median <- G21 %>%
  group_by(nx * nfacet) %>%
  summarise(actual = median(value))


glm_fit <- glm(actual ~ log(`nx * nfacet`),
  family = Gamma(link = "inverse"),
  data = G21_median
)

intercept <- glm_fit$coefficients[1]
slope <- glm_fit$coefficients[2]


G21_sd <- G21 %>%
  mutate(wpd_glm = (value - (1 / (intercept + slope * log(nx * nfacet)
  )
  )
  ))

scale_fac <- 1 / G21_sd$wpd_glm %>% sd()


G21_residual <- G21 %>%
  ggplot(aes(
    x = log(nx * nfacet),
    y = (value - (1 / (intercept + slope * log(nx * nfacet)
    )
    )
    )
  )) +
  geom_point(alpha = 1, size = 1) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "blue") +
  ylab("residuals") +
  ggtitle("Residual plot")


G21_glm <- G21 %>%
  mutate(
    wpd_glm = (value - (1 / (intercept + slope * log(nx * nfacet)
    )
    )
    ),
    wpd_glm_scaled = ((wpd_glm * 320))
  )


G21_dist <- G21_glm %>%
  ggplot() +
  geom_density(aes(x = wpd_glm),
    fill = "blue"
  ) +
  facet_grid(nx ~ nfacet,
    labeller = "label_both"
  ) +
  theme(legend.position = "bottom")


G21_residual +
  theme(
    axis.text = element_text(size = 32),
    strip.text = element_text(
      size = 32,
      margin = margin()
    ),
    axis.title = element_text(size = 32),
    legend.title = element_text(size = 32),
    legend.text = element_text(size = 32),
    legend.position = "bottom",
    plot.title = element_text(size = 32)
  ) +
  theme(plot.title = element_text(hjust = 0.5))

## ----glm-dist
G21_dist
## ----allplots

knitr::include_graphics("figs/allplots.png")

## ----graphical map

knitr::include_graphics("figs/graphical_map.png")

## ----data-structure2

knitr::include_graphics("figs/data-structure2.png")


## ----clash

VIC <- tsibbledata::vic_elec

clash <- VIC %>%
  create_gran("month_year") %>%
  filter(month_year %in% c("Jan", "Jul", "Nov")) %>%
  prob_plot("month_year",
    "day_year",
    response = "Demand",
    plot_type = "quantile",
    quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9),
    symmetric = FALSE
  ) + ggtitle("") + theme_remark() +
  scale_x_discrete(breaks = seq(0, 364, 40)) +
  scale_color_brewer(palette = "Dark2") +
  ylab("Energy consumption (kwh)") +
  theme(
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

## ----noclash

VIC <- tsibbledata::vic_elec

noclash <- VIC %>%
  create_gran("month_year") %>%
  filter(month_year %in% c("Jan", "Mar", "July", "Dec")) %>%
  prob_plot("month_year", "day_week",
    response = "Demand",
    plot_type = "lv"
  ) + ggtitle("") + theme_remark() +
  theme(
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 14)
  ) +
  scale_x_discrete(breaks = c("Sun", "Wed", "Fri")) + ylab("")

## ----bothclash2
clash + noclash

## ----same-scale

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
  pivot_longer(
    cols = c(3, 7),
    names_to = "type_estimate",
    values_to = "value_estimate"
  )
G21_all_data$type_estimate <- factor(G21_all_data$type_estimate, levels = c("wpd_permutation", "wpd_glm_scaled"))


G21_all_data %>%
  filter(type_estimate %in% c("wpd_glm_scaled", "wpd_permutation")) %>%
  ggplot() +
  geom_density(aes(
    x = value_estimate,
    fill = type_estimate
  ), alpha = 0.5) +
  facet_grid(nx ~ nfacet,
    labeller = "label_both"
  ) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#D55E00", "#0072B2")) +
  xlab("wpd_norm2") +
  scale_x_continuous(breaks = c(-5, -3, 0, 3, 5))

## ----linear-scale-8new

library(scales)
library(tidyquant)
library(gghighlight)
library(lubridate)
library(here)
library(tidyverse)
library(gghighlight)

elec <- read_rds(here("data/elec_all-8.rds")) %>%
  dplyr::filter(date(reading_datetime) >= ymd("20190701"), date(reading_datetime) < ymd("20191231"), meter_id == 1) %>%
  select(-meter_id) %>%
  rename(
    "id" = "household_id",
    "date_time" = "reading_datetime"
  ) %>%
  mutate(date = date(date_time))


elec_linear <- elec %>%
  ggplot() +
  geom_line(aes(x = date_time, y = kwh), alpha = 0.7) +
  facet_wrap(~id,
    nrow = 8, labeller = "label_both",
    strip.position = "right",
    scales = "free_y"
  ) +
  ggtitle("Jun-19 to Dec-19 demand") +
  ylab("Demand (kwh)")

elec_zoom <- elec %>%
  as_tibble() %>%
  filter(date > as.Date("2019-09-01") & date < (as.Date("2019-09-30"))) %>%
  ggplot(aes(x = date_time, y = kwh)) +
  geom_line(size = 0.1, colour = "blue") +
  facet_wrap(~id,
    scales = "free_y",
    ncol = 1,
    strip.position = "right"
  ) +
  gghighlight(date > as.Date("2019-09-15") & date < (as.Date("2019-09-21")), unhighlighted_params = list(colour = "black")) +
  ggtitle("Zoomed in for Sep-19") +
  ylab("Demand (kwh)")

p <- elec_linear + elec_zoom

p

## ----search-gran

elec %>%
  search_gran(
    lowest_unit = "hour",
    highest_unit = "month",
    filter_in = "wknd_wday",
    filter_out = "fortnight"
  )

## ---- harmony
elec %>% harmony(
  ugran = "month",
  filter_in = "wknd_wday",
  filter_out = c("hhour", "fortnight")
)

## ---- select-harmonies
elec_select_harmony <- parallel::mclapply(1:8, function(x) {
  data_id <- elec_split %>%
    magrittr::extract2(x) %>%
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
# toc()


elec_harmony_all <- elec_select_harmony %>%
  bind_rows(.id = "id") %>%
  mutate(facet_variable = case_when(
    facet_variable == "hour_day" ~ "hod",
    facet_variable == "day_month" ~ "dom",
    facet_variable == "day_week" ~ "dow",
    facet_variable == "week_month" ~ "wom",
    facet_variable == "wknd_wday" ~ "wdwnd"
  )) %>%
  mutate(x_variable = case_when(
    x_variable == "hour_day" ~ "hod",
    x_variable == "day_month" ~ "dom",
    x_variable == "day_week" ~ "dow",
    x_variable == "week_month" ~ "wom",
    x_variable == "wknd_wday" ~ "wdwnd"
  )) %>%
  mutate(id = paste("id", id, sep = " ")) %>%
  group_by(id) %>%
  mutate(rank = row_number())

select_split <- str_split(elec_harmony_all$select_harmony, " ", simplify = TRUE)[, 2]

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
    select_split == "**" ~ paste(rank, "**", sep = " "),
    select_split == "*" ~ paste(rank, "*", sep = " "),
    select_split == "" ~ paste(rank, "", sep = " ")
  ))

elec_rank <- elec_sig_split %>%
  select(-c(6, 7, 9, 10)) %>%
  pivot_wider(
    names_from = id,
    values_from = rank
  ) %>%
  rename(
    "facet variable" = "facet_variable",
    "x variable" = "x_variable"
  ) %>%
  select(-facet_levels, -x_levels)

## ----heatplot-new


elec_sig_split <- read_rds("data/elec_sig_split.rds")

heatplot <- elec_sig_split %>%
  mutate(significance_95 = if_else(significant %in% c("high", "highest"), "*", "")) %>%
  ggplot(aes(
    x = x_variable,
    y = facet_variable
  )) +
  geom_tile(aes(fill = wpd)) +
  # color = as.factor(significance_95)),
  # size = 0.3) +
  geom_text(aes(label = significance_95), color = "#42275a", size = 5) +
  scale_fill_gradient(low = "#BFD5E3", high = "#B02A0F") +
  # scale_fill_manual(palette = "Dark2") +
  # scale_colour_manual(values = c("white","red")) +
  theme(legend.position = "bottom") +
  # coord_fixed() +
  guides(fill = guide_legend()) +
  theme_void() +
  theme_gray(base_size = 12, base_family = "Times") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "bottom"
  ) +
  ggtitle("") +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank(), plot.margin = unit(c(0, -2, 0, 0), "cm")
  ) +
  theme(
    panel.background = element_rect(fill = "white", colour = "grey"),
    plot.margin = unit(c(1, -5, 1, 1), "cm")
  )





elec <- read_rds(here("data/elec_all-8.rds")) %>%
  dplyr::filter(date(reading_datetime) >= ymd("20190701"), date(reading_datetime) < ymd("20191231"), meter_id == 1) %>%
  select(-meter_id) %>%
  rename(
    "id" = "household_id",
    "date_time" = "reading_datetime"
  )

elec_zoom <- elec %>%
  as_tibble() %>%
 # dplyr::filter(date(date_time) > as.Date("2019-09-01") & date(date_time) < (as.Date("2019-09-30"))) %>%
  ggplot(aes(x = date_time, y = kwh)) +
  # geom_point(size = 0.1, colour = "black", alpha = 0.3) +
  geom_line(size = 0.4, colour = "blue") +
  facet_wrap(~id,
    scales = "free_y",
    ncol = 1,
    strip.position = "right",
    labeller = "label_both"
  ) +
  xlab("Time [30m]") +
  theme_grey() +
  ylab("") +
  ggtitle("") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = unit(c(1, 1, 1, -2), "cm")
  )

p <- heatplot + facet_grid(id ~ .) + coord_fixed() + elec_zoom +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  plot_layout(widths = c(1, 3))

p

ggsave("heatplot.png", p, "png", path = "./figs/", dpi = 700, height = 25, width = 42, unit = "cm")
#
# ggsave("eleclinear8.tiff", p, units = "in",  path = "./figs/", width = 5,  device = "tiff", dpi = 700)

# ggsave("heatplot.png", p, "png", path = "./figs/", dpi= 300, height = 25, unit = "cm")

# knitr::include_graphics("figs/eleclinear8.tiff")




## ----heatplot-call3
ggsave("heatplot.png", p, "png", path = "./figs/", dpi = 300, height = 20, unit = "cm")

knitr::include_graphics("figs/heatplot.png")

## ----heatplot-only
heatplot


## ----gravitas-plot

id1_tsibble <- elec %>%
  filter(id == 1)

p1 <- id1_tsibble %>%
  prob_plot("hour_day",
    "wknd_wday",
    response = "kwh",
    plot_type = "quantile",
    symmetric = FALSE,
    quantile_prob = c(0.25, 0.5, 0.75), size = 5
  ) +
  ggtitle("a) hod vs wdwnd (***)") +
  scale_colour_brewer(name = "", palette = "Set2") +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 7, margin = margin()), panel.background = element_blank()
  )


p2 <- id1_tsibble %>%
  prob_plot("wknd_wday",
    "hour_day",
    response = "kwh",
    plot_type = "quantile",
    symmetric = FALSE,
    quantile_prob = c(0.25, 0.5, 0.75)
  ) +
  ggtitle("c) wdwnd vs hod (**)") +
  scale_colour_brewer(name = "", palette = "Set2") +
  theme(
    legend.position = "none",
    strip.text = element_text(
      size = 7,
      margin = margin()
    )
  ) +
  scale_x_discrete(breaks = seq(0, 23, 5))

p3 <- id1_tsibble %>%
  prob_plot("week_month",
    "wknd_wday",
    response = "kwh",
    plot_type = "quantile",
    symmetric = FALSE,
    quantile_prob =
      c(0.25, 0.5, 0.75)
  ) +
  scale_colour_brewer(name = "", palette = "Set2") +
  theme(
    strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))
  ) + theme(
    legend.position = "bottom",
    strip.text = element_text(size = 7, margin = margin())
  ) +
  ggtitle("d) wom vs wdwnd (insignificant)")

p4 <- id1_tsibble %>%
  prob_plot("day_month",
    "hour_day",
    response = "kwh",
    plot_type = "quantile",
    symmetric = FALSE,
    quantile_prob =
      c(0.25, 0.5, 0.75)
  ) +
  scale_colour_brewer(name = "", palette = "Set2") +
  theme(
    strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))
  ) + theme(
    legend.position = "bottom",
    strip.text = element_text(size = 7, margin = margin())
  ) +
  ggtitle("b) dom vs hod (***)") +
  scale_x_discrete(breaks = seq(0, 31, 5))

(p1 + p4) / (p2 + p3) + theme_classic()

## ----gravitas-plot1
knitr::include_graphics("figs/plot1.png")

## ----gravitas-plot2
knitr::include_graphics("figs/plot2.png")


## ----gravitas-plot3
knitr::include_graphics("figs/plot3.png")

## ----gravitas-plot4
knitr::include_graphics("figs/plot4.png")



## ---- theme
# knitr::include_graphics("figs/theme.png")
knitr::include_graphics("figs/theme-roadmap2.png")

## ---- motivation
knitr::include_graphics("figs/motivation2.png")

## ----check-new
elec <- read_rds(here("data/elec_all-8.rds")) %>%
  dplyr::filter(date(reading_datetime) >= ymd("20190701"), date(reading_datetime) < ymd("20191231"), meter_id == 1) %>%
  select(-meter_id) %>%
  rename(
    "id" = "household_id",
    "date_time" = "reading_datetime"
  )

id1_tsibble <- elec %>%
  filter(id == 1)

id1_tsibble1 <- id1_tsibble %>%
  create_gran("hour_day") %>%
  create_gran("wknd_wday") %>%
  as_tibble() %>%
  select(hour_day, wknd_wday, kwh)

# prob <- seq(0.01,0.99,by=0.1)
# prob <- c(0.01,0.1, 0.25, 0.5, 0.75, 0.9, 0.99)
prob <- c(0.25, 0.5, 0.75)

# id1_tsibble1 %>%
#   group_by(hour_day, wknd_wday) %>%
#   summarise()

nhour <- unique(id1_tsibble1$hour_day)
nwday <- unique(id1_tsibble1$wknd_wday)

percetile_data <- lapply(
  seq_len(length(nhour)),
  function(x) {
    lapply(seq_len(length(nwday)), function(y) {
      data <- id1_tsibble1 %>% filter(hour_day == nhour[x], wknd_wday == nwday[y])
      quantile(data$kwh, prob = prob, type = 8)
    }) %>% bind_rows(.id = "x")
  }
) %>% bind_rows(.id = "hod")


all_data <- percetile_data %>%
  pivot_longer(-c(1:2),
    values_to = "values", names_to = "percentiles"
  )

all_data$hod <- as_factor(all_data$hod)
all_data$x <- as_factor(all_data$x)

levels(all_data$x) <- c("weekday", "weekend")


p1 <- all_data %>%
  ggplot(aes(x = x, y = values, colour = percentiles, group = percentiles)) +
  geom_line(size = 1) +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10, margin = margin(b = 0, t = 0)),
    panel.grid.major = element_blank()
  ) +
  scale_color_viridis_d(direction = -1) +
  facet_wrap(~hod, labeller = "label_both") +
  ylab("demand (kwh)") +
  xlab("") + ggtitle("a) hod vs wdwnd (***)") 

# ggsave("plot1.png", p1, "png", path = "./figs/", dpi= 300, height = 19, unit = "cm")

## ---- check2-new
library(tidyverse)
library(ggplot2)
library(gravitas)

id1_tsibble1 <- id1_tsibble %>%
  create_gran("day_month") %>%
  create_gran("hour_day") %>%
  as_tibble() %>%
  select(day_month, hour_day, kwh)

# prob <- seq(0.01,0.99,by=0.1)
# prob <- c(0.01,0.1, 0.25, 0.5, 0.75, 0.9, 0.99)
prob <- c(0.25, 0.5, 0.75)

# id1_tsibble1 %>%
#   group_by(day_month, hour_day) %>%
#   summarise()

nhour <- unique(id1_tsibble1$day_month)
nwday <- unique(id1_tsibble1$hour_day)

percetile_data <- lapply(
  seq_len(length(nhour)),
  function(x) {
    lapply(seq_len(length(nwday)), function(y) {
      data <- id1_tsibble1 %>% filter(day_month == nhour[x], hour_day == nwday[y])
      quantile(data$kwh, prob = prob, type = 8)
    }) %>% bind_rows(.id = "x")
  }
) %>% bind_rows(.id = "dom")


all_data <- percetile_data %>%
  pivot_longer(-c(1:2),
    values_to = "values", names_to = "percentiles"
  )

all_data$dom <- as_factor(all_data$dom)
all_data$x <- as_factor(all_data$x)

p2 <- all_data  %>%
  filter(dom!= 31) %>% 
  ggplot(aes(x = x, y = values, colour = percentiles, group = percentiles)) +
  geom_line(size = 1) +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10, margin = margin(b = 0, t = 0)),
    panel.grid.major = element_blank()
  ) +
  scale_color_viridis_d(direction = -1) +
  facet_wrap(~dom, labeller = "label_both") +
  scale_x_discrete(breaks = seq(0, 31, 5)) +
  xlab("hour_day") +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))
  ) +
  ylab("") + ggtitle("b) dom vs hod (***)") 

# ggsave("plot2.png", p2, "png", path = "./figs/", dpi= 300, height = 19, unit = "cm")


## ---- check3-new
library(tidyverse)
library(ggplot2)
library(gravitas)

id1_tsibble1 <- id1_tsibble %>%
  create_gran("wknd_wday") %>%
  create_gran("hour_day") %>%
  as_tibble() %>%
  select(wknd_wday, hour_day, kwh)

# prob <- seq(0.01,0.99,by=0.1)
# prob <- c(0.01,0.1, 0.25, 0.5, 0.75, 0.9, 0.99)
prob <- c(0.25, 0.5, 0.75)

nhour <- unique(id1_tsibble1$wknd_wday)
nwday <- unique(id1_tsibble1$hour_day)

percetile_data <- lapply(
  seq_len(length(nhour)),
  function(x) {
    lapply(seq_len(length(nwday)), function(y) {
      data <- id1_tsibble1 %>% filter(wknd_wday == nhour[x], hour_day == nwday[y])
      quantile(data$kwh, prob = prob, type = 8)
    }) %>% bind_rows(.id = "x")
  }
) %>% bind_rows(.id = "f")


all_data <- percetile_data %>%
  pivot_longer(-c(1:2),
    values_to = "values", names_to = "percentiles"
  )

all_data$f <- as_factor(all_data$f)

all_data$x <- as_factor(all_data$x)
levels(all_data$f) <- c("weekday", "weekend")

p3 <- all_data %>%
  filter(f != 31) %>%
  ggplot(aes(x = x, y = values, colour = percentiles, group = percentiles)) +
  geom_line(size = 1) +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10, margin = margin(b = 0, t = 0)),
    panel.grid.major = element_blank(),
  ) +
  scale_color_viridis_d(direction = -1) +
  facet_wrap(~f) +
  scale_x_discrete(breaks = seq(0, 24, 5)) +
  xlab("hour_day") +
  theme(
    panel.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))
  ) +
  ylab("demand (kwh)") + ggtitle("c) wdwnd vs hod (**)")

# ggsave("plot3.png", p3, "png", path = "./figs/", dpi= 300, height = 19, unit = "cm")


##----check4-new
library(tidyverse)
library(ggplot2)
library(gravitas)

id1_tsibble1 <- id1_tsibble %>%
  create_gran("week_month") %>%
  create_gran("wknd_wday") %>%
  as_tibble() %>%
  select(week_month, wknd_wday, kwh)

# prob <- seq(0.01,0.99,by=0.1)
# prob <- c(0.01,0.1, 0.25, 0.5, 0.75, 0.9, 0.99)
prob <- c(0.25, 0.5, 0.75)

nhour <- unique(id1_tsibble1$week_month)
nwday <- unique(id1_tsibble1$wknd_wday)

percetile_data <- lapply(
  seq_len(length(nhour)),
  function(x) {
    lapply(seq_len(length(nwday)), function(y) {
      data <- id1_tsibble1 %>% filter(week_month == nhour[x], wknd_wday == nwday[y])
      quantile(data$kwh, prob = prob, type = 8)
    }) %>% bind_rows(.id = "x")
  }
) %>% bind_rows(.id = "wom")


all_data <- percetile_data %>%
  pivot_longer(-c(1:2),
    values_to = "values", names_to = "percentiles"
  )

all_data$wom <- as_factor(all_data$wom)
all_data$x <- as_factor(all_data$x)

p4 <- all_data %>%
  filter(wom != 5) %>%
  ggplot(aes(x = x, y = values, colour = percentiles, group = percentiles)) +
  geom_line(size = 1) +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10, margin = margin(b = 0, t = 0)),
    panel.grid.major = element_blank()
  ) +
  scale_color_viridis_d(direction = -1) +
  facet_wrap(~wom, labeller = "label_both") +
  xlab("") +
  scale_x_discrete(labels = c("weekday", "weekend")) +
  ylab("") + ggtitle("d) wom vs wdwnd (insignificant)")


# ggsave("plot4.png", p4, "png", path = "./figs/", dpi= 300, height = 19, unit = "cm")


## ---- gravitas-plot-new3

knitr::include_graphics("figs/plot_final.png")


#  p_final <- (p1 + p2)/(p3 + p4) + theme_fonts()
#  p_final
# ggsave("plot_final4.png", p_final, "png", path = "./figs/", dpi= 300, height = 25, width = 44, unit = "cm")

# knitr::include_graphics("figs/plot_final.png")



## ----heatplot-new-4


elec_sig_split <- read_rds("data/elec_sig_split.rds") %>% 
  filter(id %in% c("id 5", "id 6", "id 7", "id 8"))

heatplot <- elec_sig_split %>%
  mutate(significance_95 = if_else(significant %in% c("high", "highest"), "*", "")) %>%
  ggplot(aes(
    x = x_variable,
    y = facet_variable
  )) +
  geom_tile(aes(fill = wpd)) +
  # color = as.factor(significance_95)),
  # size = 0.3) +
  geom_text(aes(label = significance_95), color = "#42275a", size = 5) +
  scale_fill_gradient(low = "#BFD5E3", high = "#B02A0F") +
  # scale_fill_manual(palette = "Dark2") +
  # scale_colour_manual(values = c("white","red")) +
  theme(legend.position = "bottom") +
  # coord_fixed() +
  guides(fill = guide_legend()) +
  theme_void() +
  theme_gray(base_size = 12, base_family = "Times") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "left"
  ) +
  ggtitle("") +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank(), plot.margin = unit(c(0, -2, 0, 0), "cm")
  ) +
  theme(
    panel.background = element_rect(fill = "white", colour = "grey"),
    plot.margin = unit(c(1, -1, 1, 1), "cm")
  )


elec <- read_rds(here("data/elec_all-8.rds")) %>% 
  dplyr::filter(date(reading_datetime) >= ymd("20190701"), date(reading_datetime) < ymd("20191231"), meter_id == 1) %>%
  select(-meter_id) %>%
  rename(
    "id" = "household_id",
    "date_time" = "reading_datetime"
  ) %>%
  filter(id %in% c("5", "6", "7", "8"))

elec_zoom <- elec %>%
  as_tibble() %>%
  #dplyr::filter(date(date_time) > as.Date("2019-09-01") & date(date_time) < (as.Date("2019-10-30"))) %>%
  ggplot(aes(x = date_time, y = kwh)) +
  # geom_point(size = 0.1, colour = "black", alpha = 0.3) +
  geom_line(size = 0.4, colour = "blue") +
  facet_wrap(~id,
             scales = "free_y",
             ncol = 1,
             strip.position = "right",
             labeller = "label_both"
  ) +
  xlab("Time [30m]") +
  theme_grey() +
  ylab("") +
  ggtitle("") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = unit(c(1, 1, 1, -1), "cm")
  )

p <- heatplot + facet_grid(id ~ .) + 
  coord_fixed() +
  theme_fonts() + 
  elec_zoom +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  plot_layout(widths = c(1, 3)) +
  theme_fonts()

p

ggsave("heatplot-4-new.png", p, "png", path = "./figs/", dpi = 700, height = 25, width = 42, unit = "cm")

##----demography
demography <- tibble(id = c(5,6, 7, 8), 
                     profession = c("mixed", "mixed", "academia", "industry"),
                     total_members = c(2, 2, 2, 6),
                     kids = c("no", "no", "no", "yes"),
                     old_parents = c("no", "no", "no", "yes"),
                     PhD_student = c( "yes", "yes", "yes", "no"))

demography

##----calendar

elec <- read_rds(here("paper/data/elec.rds")) %>% 
  filter(date >= ymd("20180101"), date < ymd("20180701"))
rdbl <- c("Weekday" = "#d7191c", "Weekend" = "#2c7bb6")

elec <- elec %>% 
  mutate(
    wday = wday(date, label = TRUE, week_start = 1),
    weekday = if_else(wday %in% c("Sat", "Sun"), "Weekend", "Weekday")
  )
p_cal_elec <- elec %>% 
  filter(id %in% c(2, 4)) %>% 
  frame_calendar(x = time, y = kwh, date = date, nrow = 1) %>% 
  ggplot(aes(x = .time, y = .kwh, group = date)) +
  geom_line(aes(colour = as.factor(id)), size = 0.5) +
  scale_colour_brewer(name = "", palette = "Dark2", direction = 1) +
  facet_grid(id ~ ., labeller = label_both) +
  theme(legend.position = "bottom")
prettify(p_cal_elec, size = 2.5, label.padding = unit(0.1, "lines"))

