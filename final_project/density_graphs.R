library(tidyverse)
library(RColorBrewer)
library(maps)
library(mapproj)
library(usmap)
library(cowplot)
library(socviz)
# library(rvest)

# page <- read_html("https://state.1keydata.com/states-by-size.php")
# 
# state <- page %>%
#   html_nodes("td+ td tr+ tr td:nth-child(2)") %>%
#   html_text()
# 
# state <- c(state, "District of Columbia")
# 
# mileage <- page %>%
#   html_nodes("td+ td tr+ tr td~ td+ td") %>%
#   html_text() %>%
#   str_replace("\\,", "") %>%
#   as.numeric()
# 
# mileage <- c(mileage, 68)
# 
# sq_mileage <- tibble(
#   state = state,
#   mileage = mileage
# )
# 
# sq_mileage %>% write_csv("data/mileage.csv")

sq_mileage <- read_csv("data/mileage.csv")
DNKN_loc <- read_csv("data/DunkinAvgLocations#2.csv")
SBUX_loc <- read_csv("data/StarbuckAvgLocations#2.csv") %>%
  rename(n = count)

DNKN_loc <- DNKN_loc %>%
  distinct(STATE, n) %>%
  rename(state = STATE)
SBUX_loc <- SBUX_loc %>%
  distinct(STATE, n) %>%
  rename(state = STATE)

DNKN_loc <- left_join(us_map(region = "states") %>%
                        rename(state = abbr), DNKN_loc, by = "state")
SBUX_loc <- left_join(us_map(region = "states") %>%
                        rename(state = abbr), SBUX_loc, by = "state")

sq_mileage <- sq_mileage %>%
  rename(full = state,
         state_area = mileage)

DNKN_loc <- left_join(DNKN_loc, sq_mileage, by = "full")
SBUX_loc <- left_join(SBUX_loc, sq_mileage, by = "full")

DNKN_loc <- DNKN_loc %>%
  mutate(density = n/state_area)
SBUX_loc <- SBUX_loc %>%
  mutate(density = n/state_area)

DNKN_loc %>%
  ggplot(mapping = aes(x = x, y = y, group = group, fill = density)) +
  geom_polygon() +
  coord_equal() +
  theme_map() +
  scale_fill_distiller(palette = "Set1")
SBUX_loc %>%
  ggplot(mapping = aes(x = x, y = y, group = group, fill = density)) +
  geom_polygon() +
  coord_equal() +
  theme_map() +
  scale_fill_distiller(palette = "Set1")

DNKN_loc <- DNKN_loc %>%
  filter(state %nin% c("DC"))
SBUX_loc <- SBUX_loc %>%
  filter(state %nin% c("DC"))

DNKN_loc %>%
  ggplot(mapping = aes(x = x, y = y, group = group, fill = density)) +
  geom_polygon() +
  coord_equal() +
  theme_map() +
  scale_fill_distiller(palette = "Set1")
SBUX_loc %>%
  ggplot(mapping = aes(x = x, y = y, group = group, fill = density)) +
  geom_polygon() +
  coord_equal() +
  theme_map() +
  scale_fill_distiller(palette = "Set1")

