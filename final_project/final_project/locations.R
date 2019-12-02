library(tidyverse)
library(cowplot)
library(socviz)
library(usmap)


#gather data from csv files
starbuck_local<- read_csv("data/directory.csv")
starbuck_zip<-read_csv("data/ZIP-COUNTY-FIPS-2018.csv")

#getting states dataset
us_states<-map_data("state")

#chaging dataset zip to numeric
starbuck_zip<-starbuck_zip %>% 
  mutate(ZIP=as.numeric(ZIP)) %>% 
  distinct(CITY, .keep_all = TRUE)

#filtering by country and renaming
starbuck_local <- starbuck_local %>% 
  filter(Country == "US") %>% 
  rename(
    CITY = City,
    STATE = `State/Province`
  )

#joining datasets by city
data<-left_join(starbuck_local,starbuck_zip,by=c("CITY","STATE"))

#grouping by state and removing na values
data<- data %>% 
  group_by(STATE) %>% 
  count() %>%
  na.omit() 

#renaming states to match join function
us_states<-us_map("states") %>% 
  rename(STATE=abbr)

#left joining and renaming variables
data<-left_join(us_states,data,by='STATE') %>% 
  rename(count = n)

temp_data <- data %>% 
  select(STATE, count) %>% 
  group_by(STATE) %>% 
  summarise(count = mean(count))

coord <- us_states %>% 
  select(x, y, STATE) %>% 
  group_by(STATE) %>% 
  summarise(
    x_avg = mean(x),
    y_avg = mean(y))

new_data <- merge(temp_data, coord, by = "STATE" )

final_data <- merge(new_data, us_states, by = "STATE" )

#plot data for locations
centroid <- aggregate(data=final_data, cbind(x_avg, y_avg) ~ count + group, FUN=mean) 
data %>% 
  ggplot() +
  geom_polygon(mapping = aes(x=x,y=y,group=group,fill=count), color = "lightgray", size = 0.5) +
  coord_equal()+
  theme_map()+
  theme(panel.background = element_blank()) + ggtitle("US Starbuck's Location", subtitle = "Average # of Stores") + labs(fill = "Count")+
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5),plot.subtitle =  element_text(hjust = 0.5)) +geom_text(data = centroid, mapping = aes(x_avg,y_avg, group = group, label = count), color = "black", inherit.aes = FALSE)+ scale_fill_gradient(low = "orange", high = "purple", guide = guide_legend())


write_csv(data, "data/StarbuckAvgLocations#1.csv")
write_csv(final_data, "data/StarbuckAvgLocations#2.csv")

