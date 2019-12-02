library(tidyverse)
library(RColorBrewer)
library(maps)
library(mapproj)
library(usmap)
library(cowplot)
library(socviz)

sbuxloc <- read_csv("data/sbuxlocations.csv")
dnknloc <- read_csv("data/dnknlocations.csv", col_names = FALSE)

dnknloc <- dnknloc %>%
  rename(Longitude = X1,
         Latitude = X2,
         Specs = X3,
         Location = X4)
sbuxloc <- sbuxloc %>%
  filter(Country %in% c("US"))

dnknloc <- dnknloc %>%
  mutate(store = "Dunkin")
sbuxloc <- sbuxloc %>%
  mutate(store = "Starbucks")

dnknloc <- dnknloc %>%
  separate(Specs, c("Specs", "State"), sep = ",")

dnknloc <- dnknloc %>%
  mutate(State = dnknloc %>%
           pull(State) %>%
           str_replace_all("\\ ", ""))

dnkn_sbux <- tibble(
  longitude = c(dnknloc %>%
                  pull(Longitude), sbuxloc %>%
                  pull(Longitude)),
  latitude = c(dnknloc %>%
                 pull(Latitude), sbuxloc %>%
                 pull(Latitude)),
  store = c(dnknloc %>%
              pull(store), sbuxloc %>%
              pull(store)),
  state = c(dnknloc %>%
              pull(State), sbuxloc %>%
              pull(`State/Province`))
)

values <- tibble(
  abbreviation = dnkn_sbux$state,
  x = dnkn_sbux$longitude,
  y = dnkn_sbux$latitude
)

scale_coord <- function(abbreviation, x, y) {
  library(usmap)
  values <- tibble(
    abbreviation = abbreviation,
    x = x,
    y = y
  )
  usMap <- us_map(regions = "states")
  scaler <- usMap %>%
    group_by(abbr) %>%
    summarize(max.x = max(x),
              max.y = max(y),
              min.x = min(x),
              min.y = min(y))
  scaler <- scaler %>%
    mutate(variation.x = max.x - min.x,
           variation.y = max.y - min.y)
  base <- values %>%
    group_by(abbreviation) %>%
    summarize(max.x = max(x),
              max.y = max(y),
              min.x = min(x),
              min.y = min(y))
  base <- base %>%
    mutate(variation.x = max.x - min.x,
           variation.y = max.y - min.y)
  longitude <- c()
  latitude <- c()
  indexes <- c()
  count <- 1
  for(state in values$abbreviation) {
    for(abbrs in scaler$abbr) {
      if(state == abbrs) {
        indexes <- c(indexes, count)
      }
      count <- count + 1
    }
    count <- 1
  }
  count <- 1
  for(index in indexes) {
    longitude <- c(longitude, ((values$x[count]-base$min.x[index])/base$variation.x[index]) * scaler$variation.x[index] + scaler$min.x[index])
    latitude <- c(latitude, ((values$y[count]-base$min.y[index])/base$variation.y[index]) * scaler$variation.y[index] + scaler$min.y[index])
    count <- count + 1
  }
  return_tibble <- tibble(
    state = values$abbreviation,
    long = longitude,
    lat = latitude
  )
  return(return_tibble)
}

modified_values <- scale_coord(dnkn_sbux$state, dnkn_sbux$longitude, dnkn_sbux$latitude) %>%
  mutate(store = dnkn_sbux$store)

ggplot() +
  geom_polygon(data = us_map(regions = "states"), mapping = aes(x = x, y = y, group = group), color = "gray", fill = "cornsilk") +
  geom_point(data = modified_values, mapping = aes(x = long, y = lat, color = store), alpha = 0.1)

ggplot() +
  geom_point(data = dnkn_sbux %>%
               filter(state %nin% c("HI", "AK")), mapping = aes(x = longitude, y = latitude, color = store), alpha = 0.1)

