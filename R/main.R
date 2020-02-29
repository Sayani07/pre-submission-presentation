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
#remotes::install_github("njtierney/palap")
library(palap)

load("data/sm_cust50.Rdata")

smart_meter50 <- sm_cust50 %>% select(customer_id, 
         reading_datetime,
         general_supply_kwh, 
         everything())

data_cust1 <- smart_meter50 %>% filter(customer_id == 10017936)


cricket <- read_rds("data/cricket_tsibble.rds")%>%  
  select(season, 
         match_id,
         inning,
         over,
         ball,
         winner,
         total_runs,
         everything())

cricket_tsibble <- gravitas::cricket %>%
  mutate(data_index = row_number()) %>%
  as_tsibble(index = data_index)

hierarchy_model <- tibble::tibble(
  units = c("ball", "over", "inning", "match"),
  convert_fct = c(6, 20, 2, 1)
)


##----motivation1


data_cust1%>% ggplot() + geom_line(aes(x =reading_datetime, y = general_supply_kwh), color = "#1B9E77")+ theme(legend.position = "bottom")


##----motivation2
data_cust1 %>%  ggplot() + geom_line(aes(x =reading_datetime, y = general_supply_kwh, color = customer_id), color = "#D95F02") + theme(legend.position = "bottom")

##----motivation3

# smart_p <- smart_meter50 %>%  ggplot() + geom_line(aes(x =reading_datetime, y = general_supply_kwh, color = customer_id))  + theme(legend.position = "None")
# 
# smart_anim <-  smart_p + gganimate::transition_states(customer_id)+ labs(title = "{closest_state}")
# 
# gganimate::animate(smart_anim, fps = 10, width = 1000, height = 600)
# 
# 
# anim_save("images/smart_allcust.gif")

knitr::include_graphics("images/smart_allcust.gif")


##----motivation4
smart_meter50 %>%
  filter(customer_id==10018250) %>%   
  ggplot() + 
  geom_line(aes(x =reading_datetime, 
                y = general_supply_kwh, 
                color = customer_id), color = "#D95F02") + 
  theme_remark()

##----motivation5
smart_meter50 %>%
  filter(customer_id==10018250) %>%
  mutate(hour_day = hour(reading_datetime)) %>% 
  ggplot() + geom_point(aes(x = hour_day, 
                            y = general_supply_kwh)) + 
  theme_remark()
  
  #ggtitle("10018254")


##----motivation6

# set.seed(1100)
# 
# sm_cust <- smart_meter50 %>% 
#   distinct(customer_id) %>%
#   .$customer_id %>% 
#   sample(size= 10)
# 
# smart_meter50 %>%
#   dplyr::filter(customer_id %in% sm_cust) %>% 
#   mutate(hour_day = hour(reading_datetime)) %>%
#   ggplot() +
#   geom_point(aes(x = hour_day, y = general_supply_kwh, color = customer_id))  + 
#   theme(legend.position = "None")

knitr::include_graphics("images/all_households.png")

# smart_p_period <- smart_meter50 %>%
#   mutate(hour_day = hour(reading_datetime)) %>%
#   ggplot() + geom_point(aes(x = hour_day, y = general_supply_kwh, color = customer_id))  +  theme(legend.position = "None")
# 
# # 
# # 
# # smart_anim_period <-  smart_p_period + gganimate::transition_states(customer_id)+ labs(title = "{closest_state}")
# # 
# # gganimate::animate(smart_anim_period, fps = 10, width = 1000, height = 600)
# # 
# # 
# # anim_save("images/smart_allcust_period.gif")
# 
# knitr::include_graphics("images/smart_allcust_period.gif")


##----motivation7
# smart_meter50  %>%
#   mutate(hour_day = hour(reading_datetime)) %>% 
#   ggplot() + geom_point(aes(x = hour_day, y = general_supply_kwh, color = customer_id)) +  theme(legend.position = "none")

# knitr::include_graphics("images/hd_allcust.png")

##----read
smart_meter50


##----search_gran


search_gran <- smart_meter50 %>%
  search_gran(lowest_unit = "hhour", highest_unit =  "month", 
              filter_out = c("fortnight", "hhour"))

knitr::kable(search_gran, row.names = TRUE) %>% kable_styling(font_size = 20)


##----create_gran

smart_meter50 %>%
  create_gran("hour_week") %>% select(customer_id, 
                                              reading_datetime,
                                              hour_week,
                                              general_supply_kwh, 
                                              everything())


##----isharmony

smart_meter50 %>% is.harmony("hour_week","day_fortnight")

##----harmony
cust1 <- smart_meter50 %>% distinct(customer_id) %>% head(10)
cust2 <- smart_meter50 %>% distinct(customer_id) %>% head(20) %>% filter(!(customer_id %in% cust1))

cust3 <- smart_meter50 %>% distinct(customer_id) %>% head(30)%>% filter(!(customer_id %in% c(cust1, cust2)))
cust4 <- smart_meter50 %>% distinct(customer_id) %>% head(10)


smart_meter50 %>% 
  harmony(ugran = "month",
          lgran = "hhour",
          filter_out = c("fortnight", "hhour")) %>% knitr::kable(row.names = TRUE) %>% kable_styling(font_size = 16)

##----granplotoverlay1


smart_meter50   %>% 
  filter(customer_id %in% cust1$customer_id) %>% 
  prob_plot("month_year", 
           "hour_day",
           plot_type = "quantile", 
           response = "general_supply_kwh",
           quantile_prob = c(0.25, 0.5, 0.75),
           overlay = TRUE) + ggtitle("") +
  scale_x_discrete(breaks = seq(0,23,2)) + 
  theme_remark() 


##----granplotoverlay2
smart_meter50   %>% 
  filter(customer_id %in% cust1$customer_id) %>% 
  prob_plot("month_year", 
           "hour_day",
           plot_type = "quantile", 
           response = "general_supply_kwh",
           quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9),
           overlay = TRUE) + ggtitle("") +
  scale_x_discrete(breaks = seq(0,23,2)) + 
  theme_remark()

##----granplotoverlay3
smart_meter50   %>% 
  filter(customer_id %in% cust1$customer_id) %>% 
  prob_plot("month_year", 
           "hour_day",
           plot_type = "quantile", 
           response = "general_supply_kwh",
           quantile_prob = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
           overlay = TRUE) + ggtitle("") +
  scale_x_discrete(breaks = seq(0,23,2)) + scale_y_sqrt(breaks = c(0.01, 0.1, 0.5, 1:3))

##----gran-advice

smart_meter50%>% 
  filter(customer_id %in% cust1$customer_id) %>% 
  gran_advice("month_year", 
            "hour_day")


##----granplotoutlier

# smart_meter50   %>% 
#   filter(customer_id %in% cust1$customer_id) %>% 
#   granplot("day_week", 
#            "day_month",
#            plot_type = "violin", 
#            response = "general_supply_kwh",
#            quantile_prob = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
#            overlay = TRUE) + ggtitle("") + 
#   theme_remark()


# smart_meter50  %>% create_gran("day_month") %>% filter(day_month %in% c(1:12)) %>% 
#   granplot("day_week", 
#            "day_month",
#            plot_type = "violin", 
#            response = "general_supply_kwh",
#            quantile_prob = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
#            overlay = TRUE) + ggtitle("") + 
#   theme_remark() + geom_boxplot(width = 0.01, color =" Blue")
# 
# smart_meter50  %>% create_gran("hour_day") %>% filter(hour_day %in% c(0:12)) %>% 
#   granplot("week_month", 
#            "hour_day",
#            plot_type = "violin", 
#            response = "general_supply_kwh",
#            quantile_prob = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
#            overlay = TRUE) + ggtitle("") + 
#   theme_remark() + geom_boxplot(width = 0.01, color =" Blue")



##----granplot

# p1 = smart_meter50 %>% 
#   mutate(Demand = general_supply_kwh*10) %>% 
#   granplot("wknd_wday", 
#            "hour_day",
#            response = "Demand",
#            plot_type = "quantile", 
#            quantile_prob = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), overlay = FALSE) +
#   ggtitle("") + 
#   scale_colour_palap_d(palette = "lajolla",
#                        begin = 0.5,
#                        end = 1, direction = -1) + 
#   scale_x_discrete(breaks = seq(0,23,2)) + 
#   theme_remark() 
# 
# anim1 <-  p1 + transition_reveal(as.numeric(hour_day)) 
# 
# gganimate::animate(anim1, fps = 10, width = 1000, height = 600)
# 
# 
# anim_save("~/Documents/YSc2019/images/palap_quantile.gif")

knitr::include_graphics("images/palap_quantile.gif")

##----cricket

glimpse(cricket)

##----cricket-glimpse

knitr::include_graphics("images/cricket_data.png")



##----cricketex


#   cricket_per_over <- cricket %>%
#     group_by(season, match_id, batting_team, bowling_team,  inning, over) %>%
#     summarise(runs_per_over = sum(total_runs), run_rate = sum(total_runs)/length(total_runs))
# 
#   cricket_tsibble_all <- cricket_per_over %>%
#     ungroup() %>%
#     mutate(data_index = row_number()) %>%
#     as_tsibble(index = data_index)
# 
# 
#   p = cricket_tsibble_all %>%
#     filter(season %in% c(2010:2015),
#            batting_team %in% c("Mumbai Indians", "Kolkata Knight Riders"),
#                                inning %in% c(1,2)) %>%
#     granplot("inning", "over",
#              hierarchy_model,
#              response = "run_rate",
#              plot_type = "lv",
#              quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9))
# 
# 
# 
# anim <-  p + gganimate::transition_states(batting_team)+ labs(title = "{next_state}",
#                                                               transition_length = 0.1,
#                                                               state_length = 2)
# 
# gganimate::animate(anim, fps = 10, width = 1000, height = 600)
# 
# 
#anim_save("~/Documents/YSc2019/images/cricketex.gif")
knitr::include_graphics("images/cricketex.gif")


##----lineartime

knitr::include_graphics("images/linear-time.png")


##----circulartime
knitr::include_graphics("images/circular.png")


##----calendar
knitr::include_graphics("images/calendar_new.jpg")


##----allplot

pbox <- smart_meter50 %>% 
  create_gran("hour_day") %>% 
  filter(hour_day %in% c(20, 10, 2, 15)) %>% 
  ggplot(aes(x = hour_day, y = log(general_supply_kwh))) +
  geom_boxplot() + ylab("") + xlab("")


pviolin <- smart_meter50 %>% 
  create_gran("hour_day") %>% 
  filter(hour_day %in% c(20, 10, 2, 15)) %>% 
  ggplot(aes(x = hour_day, y = log(general_supply_kwh))) +
  geom_violin() + ylab("") + xlab("")
  
plv <- smart_meter50 %>% 
  create_gran("hour_day") %>% 
  filter(hour_day %in% c(20, 10, 2, 15)) %>% 
  ggplot(aes(x = hour_day, y = log(general_supply_kwh))) + 
  geom_lv(aes(fill = ..LV..), outlier.colour = "red", outlier.shape = 1) +
  ylab("") + xlab("") + theme(legend.position = "None") 


mpg <- mpg %>% filter (class %in% c("compact", "midsize", "suv","minivan")) %>% 
  mutate(cls = 
           case_when(
             class == "compact" ~ "A",
             class == "midsize" ~ "B",
             class == "suv" ~ "C",
             class == "minivan"  ~ "D"))

pridge <-  ggplot(mpg, aes( hwy, cls)) + geom_density_ridges2()+  xlab("") + ylab("")


 p4_quantile <- smart_meter50 %>% 
   create_gran("hour_day") %>% 
   mutate(Demand = log(general_supply_kwh)) %>% 
   group_by(hour_day) %>%  do({
    x <- .$Demand
    map_dfr(
      .x = c(0.1, 0.25, 0.5, 0.75, 0.9),
      .f = ~ tibble(
        Quantile = .x,
        Value = quantile(x, probs = .x, na.rm = TRUE)
      )
    )
  })
  
  pquant <- p4_quantile %>% ggplot(aes(x = hour_day, y = Value, group=Quantile,  col = as.factor(Quantile))) + geom_line() +   xlab("") + ylab("") + theme(legend.position = "None") + scale_color_brewer(palette = "Dark2") +   ylab("") + xlab("") + scale_x_discrete(breaks = seq(0, 23, 5)) + ylab("")

phdr <- mpg %>% ggplot( 
         # make sure to change x to y from geom_density to geom_hdr_boxplot
         aes(y = hwy)) + 
    geom_hdr_boxplot(fill = "blue") + theme(legend.position = "none") +
  ylab("")

  
ggarrange(pbox, pviolin, plv, pridge, pquant, phdr, nrow = 2, ncol = 3, labels = c("box", "violin", "letter-value", "ridge", "quantile", "hdr-box"))


##----box
mpg <- mpg %>% filter (class %in% c("compact", "midsize", "suv","minivan", "pickup")) %>% 
                   mutate(cls = 
                        case_when(
                          class == "compact" ~ "A",
                          class == "midsize" ~ "B",
                          class == "suv" ~ "C",
                          class == "minivan"  ~ "D",
                          class == "pickup" ~"E"))
                          
                          

p <- ggplot(mpg, aes(cls, hwy))


p1 = p + geom_boxplot(fill = "white", colour = "#3366FF")+ xlab("") + ylab("")
p1 

##----violin
p2 = p + geom_violin(fill = "white", colour = "#3366FF")+  xlab("") + ylab("")
p2 
##----lv
p3 = p + geom_lv(aes(fill = ..LV..), outlier.colour = "red", outlier.shape = 1)+  xlab("") + ylab("")  + theme(legend.position = "None")
p3 

##----quantile
p4_quantile <- mpg %>% group_by(cls) %>%  do({
  x <- .$hwy
  map_dfr(
    .x = seq(0.1, 0.9, 0.1),
    .f = ~ tibble(
      Quantile = .x,
      Value = quantile(x, probs = .x, na.rm = TRUE)
    )
  )
})

p4 <- p4_quantile %>% ggplot(aes(x = cls, y = Value, group=Quantile,  col = as.factor(Quantile))) + geom_line() +   xlab("") + ylab("") + theme(legend.position = "None") + scale_color_brewer(palette = "Dark2")

p4 

#----ridge
p5 <- ggplot(mpg, aes( hwy, cls)) + geom_density_ridges2()+  xlab("") + ylab("")
p5 

#----overlay

# p4_overlay <- mpg %>% group_by(cls) %>%  do({
#   x <- .$hwy
#   map_dfr(
#     .x = c(0.1, 0.25, 0.5, 0.75, 0.9),
#     .f = ~ tibble(
#       Quantile = .x,
#       Value = quantile(x, probs = .x, na.rm = TRUE)
#     )
#   )
# })
# 
# 
# p4_spread <- p4_overlay %>% spread(Quantile, Value)
# 
# ggplot(p4_spread) %>% geom_line(x = cls, y='0.5')

##---types

#ggarrange(p1, p2, p3, p4, p5, nrow = 2, ncol =3, labels = c("box", "violin", "lv", "decile", "ridge"))

#----EDA1

data_cust1 %>% 
  create_gran("day_week") %>% 
  filter(day_week %in% c("Fri","Sat", "Sun", "Mon")) %>% 
  prob_plot("day_week",
            "hour_day",
            plot_type = "quantile", symmetric = FALSE,
            quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9)) + 
            ggtitle("") + 
  scale_x_discrete(breaks = seq(0, 23, 2)) + theme_remark()

#----EDA2

data_cust1 %>% 
  create_gran("month_year") %>% 
  filter(month_year %in% c("Aug", "Dec")) %>% 
  prob_plot("month_year",
            "day_week",
            plot_type = "boxplot",
            symmetric = TRUE, quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9)) + ggtitle("") + theme_remark()

#----effectoflevel1

data_cust1 %>% 
  prob_plot("wknd_wday",
            "hour_day",
            plot_type = "ridge") + 
  ggtitle("") +
  scale_y_discrete(breaks = seq(0, 23, 2))  + theme_remark()

# 
# level2 <- data_cust1 %>% 
#   prob_plot("wknd_wday",
#             "hour_day",
#             plot_type = "violin") + 
#   ggtitle("") +
#   scale_x_discrete(breaks = seq(0, 23, 2))

#----effectoflevel2
data_cust1 %>% 
  prob_plot("wknd_wday",
            "hour_day",
            plot_type = "boxplot") + 
  ggtitle("") +
  scale_x_discrete(breaks = seq(0, 23, 2)) + theme_remark()

# 
# #----effectoflevel2
# 
# data_cust1%>% 
#   create_gran("month_year") %>% 
#   filter(month_year %in% c("Aug", "Dec")) %>% 
#   prob_plot("month_year",
#             "day_month",
#             plot_type = "violin",
#             symmetric = TRUE, quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9)) + ggtitle("") +
#   theme_remark() 




#----effectoflevel4

data_cust1 %>% 
  create_gran("month_year") %>% 
  filter(month_year %in% c("Aug", "Dec")) %>% 
  prob_plot("month_year",
            "day_month",
            plot_type = "quantile",
            symmetric = FALSE, quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9)) + ggtitle("") +
  theme_remark() 


#----effectofreverse2

data_cust1 %>%
  prob_plot("wknd_wday",
            "hour_day",
            response = "general_supply_kwh",
            plot_type = "quantile",
            symmetric = TRUE,
            quantile_prob = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99)
  ) +
  scale_y_sqrt() + ggtitle("How hours of the day progress on weekdays and weekends?")  + theme_remark() + theme(
    axis.text = element_text(size = 18),
    strip.text = element_text(size = 18),
    plot.title = element_text(size = 24)
  )

#----effectofreverse1


data_cust1  %>%
  prob_plot("hour_day",
            "wknd_wday",
            response = "general_supply_kwh",
            plot_type = "boxplot"
  ) +
  scale_y_sqrt() + ggtitle("How weekend and weekday varies for every hour of the day?") + theme_remark()+ theme(
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 18),
    plot.title = element_text(size = 24)
  )



#----clash

knitr::include_graphics("images/clash.png")


##----di

knitr::include_graphics("images/di.jpg")


##----rob

knitr::include_graphics("images/rob.jpg")


##----computation1
knitr::include_graphics("images/circular-dow.png")

##----computation2
knitr::include_graphics("images/quasi-circular-example.png")

##----computation3
knitr::include_graphics("images/aperiodic-example.png")

##----load-theme
knitr::include_graphics("images/theme.png")

##----clash

VIC <- vic_elec

VIC %>%
  create_gran("month_year") %>%
  filter(month_year %in% c("Jan", "Jul", "Nov")) %>%
  prob_plot("month_year",
            "day_year",
            response = "Demand",
            plot_type = "quantile",
            quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9),
            symmetric = FALSE) + ggtitle("") + theme_remark() + 
  scale_x_discrete(breaks = seq(0, 364, 20))

##----noclash

VIC <- vic_elec

VIC %>%
  create_gran("month_year") %>%
  filter(month_year %in% c("Jan", "Mar", "July", "Dec")) %>%
  prob_plot("month_year", "day_week",
            response = "Demand",
            plot_type = "lv") + ggtitle("") + theme_remark() +
  theme(
    axis.text = element_text(size = 16))

##----hierarchy2
library(gravitas)
library(tibble)
tibble::tibble(
  G = c("ball", "over", "inning", "match"),
  P = c(6, 20, 2, 1)
)
#knitr::kable(hierarchy_model, format = "html")



##----search_gran_cric
search_gran <- cricket_tsibble %>%
  search_gran(hierarchy_model, lowest_unit = "ball", highest_unit =  "match")

knitr::kable(search_gran, row.names = TRUE) %>% 
  kable_styling(font_size = 20)%>% 
  row_spec(0, background = 	"#808080") %>% 
  row_spec(1:6, background = "White") 

##----harmony_gran_cric

harmony_cric <- cricket_tsibble %>%
  harmony(hierarchy_model, 
          lgran = "ball",
          ugran =  "match")

knitr::kable(harmony_cric, row.names = TRUE) %>% 
  kable_styling(font_size = 20, fixed_thead = F,"striped") %>% 
  row_spec(0, background = 	"#808080") %>% 
  row_spec(1:8, background = "White") 
# FFE4E1"

##----gran-advice_cric

cricket_tsibble %>% 
  gran_advice("over_inning", 
              "inning_match",
              hierarchy_model)
##----visualise_cric
cricket_tsibble %>%
  filter(batting_team %in% c(
    "Mumbai Indians",
    "Chennai Super Kings")) %>%
  prob_plot("inning",
            "over",
            response = "runs_per_over",
            hierarchy_model,
            plot_type = "lv") +
  ggtitle("") +
  scale_fill_brewer(palette = "Dark2") + 
  scale_x_discrete(breaks = seq(1, 20, 2)) +
  theme_remark()

##----aperiodic_cric

cricket_dot_field <- cricket %>%
  mutate(
    fielding_proxy = if_else(dismissal_kind %in%
                               c("caught", "caught and bowled"), 1, 0),
    dot_ball_proxy = if_else(total_runs == 0, 1, 0),
    wicket_proxy = if_else(is.na(dismissal_kind), 0, 1)
  ) %>%
  group_by(
    season,
    match_id,
    batting_team,
    bowling_team,
    inning,
    over
  ) %>%
  summarise(
    runs_per_over = sum(total_runs),
    run_rate = sum(total_runs) / length(total_runs),
    fielding_wckts = sum(fielding_proxy),
    dot_balls = sum(dot_ball_proxy)
  ) %>%
  mutate(diff_run_rate = c(0, diff(run_rate)))

cricket_tsibble <- cricket_dot_field %>%
  ungroup() %>%
  mutate(data_index = row_number()) %>%
  as_tsibble(index = data_index)

cricket_data <- cricket_tsibble %>%
  mutate(
    field = if_else(fielding_wckts == 0, "no wickets", ">0 wickets"),
    dot = if_else(dot_balls == 0, "no dot balls", ">0 dot balls"),
    lag_field = lag(field),
    lag_dot = lag(dot)
  ) %>%
  filter(lag_field != 0, lag_dot != 0)

cricket_data %>% prob_plot("over", "lag_field",
                           hierarchy_model,
                           response = "run_rate",
                           plot_type = "violin",
) + ggtitle("Does run rate decrease in the subsequent over for at least one wicket in the last over?") + geom_boxplot(width = 0.5, aes(colour = lag_field)) + xlab("Number of wickets in last over") +
  ylim(0, 4.5) +    theme_remark() + theme(
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16, margin = margin()),
    axis.title = element_text(size = 16),
    legend.position = "none",
    plot.title =  element_text(size = 16)
  )
  


#knitr::include_graphics("images/aperiodic-cric.png")


# 
# <!-- .pull-right[ -->
#                     <!-- <br> -->
#                     <!-- ```{r computation1, out.height="100px"} -->
#                     <!-- ``` -->
#                     <!-- <br> -->
#                     <!-- <br> -->
#                     <!-- ```{r computation2, out.height="100px"} -->
#                     <!-- ``` -->
#                     <!-- <br> -->
#                     <!-- <br> -->
#                     <!-- ```{r computation3, out.height="100px"} -->
#                     <!-- ``` -->
#                     <!-- ] -->
#   


#----leveltable

knitr::include_graphics("images/levels_graphs.png")