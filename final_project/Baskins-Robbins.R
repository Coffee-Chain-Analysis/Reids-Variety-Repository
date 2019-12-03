library(tidyverse)
library(usmap)
library(cowplot)
library(socviz)

df<- read_csv("data/Baskin-Robbins_USA_CAN.csv",col_names = FALSE)
zip<-read_csv("data/ZIP-COUNTY-FIPS-2018.csv")
county_map <- county_map
us_states<-map_data("state")

zip<-zip %>% 
  mutate(ZIP=as.numeric(ZIP))

df<-df %>% 
  mutate(data=sapply(strsplit(df$X4, split=',', fixed=TRUE), function(x) (x[3])))

df<- df %>% 
  mutate(STATE=sapply(strsplit(df$data, split=' ', fixed=TRUE), function(x) (x[1])),ZIP=as.numeric(sapply(strsplit(df$data, split=' ', fixed=TRUE), function(x) (x[2]))))



data<-left_join(df,zip, by=c("ZIP")) %>% 
  group_by(STATE) %>%
  count() %>%
  na.omit()

us_states<-us_map("states") %>% 
  rename(STATE=abbr)

data<-left_join(us_states,data,by='STATE')  

temp_data <- data %>% 
  select(STATE, n) %>% 
  group_by(STATE) %>% 
  summarise(n = mean(n))

coord <- us_states %>% 
  select(x, y, STATE) %>% 
  group_by(STATE) %>% 
  summarise(
    x_avg = mean(x),
    y_avg = mean(y))


new_data <- merge(temp_data, coord, by = "STATE" )

final_data <- merge(new_data, us_states, by = "STATE" )

#plot data for locations
centroid <- aggregate(data=final_data, cbind(x_avg, y_avg) ~ n + group, FUN=mean) 

data%>% 
  ggplot() +
  geom_polygon(mapping = aes(x=x,y=y,group=group,fill=n),color = "lightgray", size = 0.5) +
  coord_equal()+
  theme_map()+
  theme(panel.background = element_blank()) + ggtitle("Baskin Robbin's Location", subtitle = "Total # of Stores") + labs(fill = "Average") + theme(legend.position="bottom", plot.title = element_text(hjust = 0.5),plot.subtitle =  element_text(hjust = 0.5))+
  scale_fill_gradient(low = "orange", high = "purple", guide = guide_legend()) + geom_text(data = centroid, mapping = aes(x_avg,y_avg, group = group, label = n), color = "black", inherit.aes = FALSE)


write_csv(final_data, "data/BaskinsRobbinsAvgLocations#2.csv")