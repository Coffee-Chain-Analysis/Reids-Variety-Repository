library(tidyverse)
library(lubridate)

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
  geom_line(mapping = aes(x = timestamp, y = close, color = stock))

temp <- D_S %>%
  mutate(year = year(timestamp)) %>%
  group_by(year, stock) %>%
  summarize(mean_close = mean(close))

temp %>%
  group_by(stock) %>%
  ggplot() +
  geom_line(mapping = aes(x = year, y = mean_close, color = stock))
