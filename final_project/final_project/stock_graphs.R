library(tidyverse)
library(lubridate)
library(zoo)

# DNKN <- read_csv("data/monthly_DNKN.csv")
# SBUX <- read_csv("data/monthly_SBUX.csv")
#
# DNKN <- DNKN %>%
#   select(timestamp, close)
# SBUX <- SBUX %>%
#   select(timestamp, close)
#
# DNKN <- DNKN %>%
#   slice(2:100)
# SBUX <- SBUX %>%
#   slice(2:100)
# 
# DNKN %>% write_csv("data/modified_DNKN.csv")
# SBUX %>% write_csv("data/modified_SBUX.csv")

DNKN <- read_csv("data/modified_DNKN.csv")
SBUX <- read_csv("data/modified_SBUX.csv")

DNKN <- DNKN %>%
  mutate(stock = "DNKN")
SBUX <- SBUX %>%
  mutate(stock = "SBUX")

D_S <- tibble(
  timestamp = c(DNKN %>%
                  pull(timestamp),
                SBUX %>%
                  pull(timestamp)),
  stock = c(DNKN %>%
              pull(stock),
            SBUX %>%
              pull(stock)),
  close = c(DNKN %>%
              pull(close),
            SBUX %>%
              pull(close))
)

D_S %>%
  group_by(stock) %>%
  ggplot() +
  geom_line(mapping = aes(x = timestamp, y = close, color = stock))+
  geom_smooth(mapping = aes(timestamp,close,color=stock),method = "lm")


SBUX<-SBUX %>% 
  arrange(timestamp)
DNKN<-DNKN %>% 
  arrange(timestamp)

Starbucks_average_growth_rate<-(tail(SBUX,1)$close-head(SBUX,1)$close)/tail(SBUX,1)$close*100/7

Dunkin_average_growth_rate<-(tail(DNKN,1)$close-head(DNKN,1)$close)/tail(DNKN,1)$close*100/7




Starbucks_intercepts<-lm(SBUX$close~SBUX$timestamp)[1]$coefficient
Starbucks_monthly_average_growth_rate<-(-atan(Starbucks_intercepts[1]/Starbucks_intercepts[2]))

Dunkin_intercepts<-lm(DNKN$close~DNKN$timestamp)[1]$coefficient
Dunkin_monthly_average_growth_rate<-(-atan(Dunkin_intercepts[1]/Dunkin_intercepts[2]))

# 
# lm(DNKN$close~DNKN$timestamp) %>% 
#   Coefficients

# 
# sim1_mod <- lm(close ~ timestamp, data = filter(D_S,stock=="DNKN"))
# coef(sim1_mod)
# 
# sim1_mod <- lm(close ~ timestamp, data = filter(D_S,stock=="SBUX"))
# coef(sim1_mod)



temp <- D_S %>%
  mutate(year = year(timestamp)) %>%
  group_by(year, stock) %>%
  summarize(mean_close = mean(close))

temp %>%
  group_by(stock) %>%
  ggplot() +
  geom_line(mapping = aes(x = year, y = mean_close, color = stock))





D_S %>%
  group_by(stock) %>%
  ggplot() +
  geom_line(mapping = aes(x = timestamp, y = close, color = stock)) +
  scale_color_manual(values = c("red", "green")) +
  geom_smooth(mapping = aes(timestamp,close,color=stock),method = "lm") +
  labs(title = "Monthly Close Values",
       x = "Year",
       y = "Close Value") +
  guides(color = guide_legend(title = "Stock Symbols")) +
  theme(plot.title = element_text(hjust = 0.5))

