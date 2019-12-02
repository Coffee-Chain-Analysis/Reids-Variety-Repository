library(tidyverse)
library(cowplot)
library(socviz)
library(usmap)
library(usmaps)

df<- read_csv("data/DD-US.csv",col_names = FALSE)
zip<-read_csv("data/ZIP-COUNTY-FIPS-2018.csv")
county_map <- county_map
us_states<-map_data("state")

zip<-zip %>% 
  mutate(ZIP=as.numeric(ZIP))
  
df <- df %>%
  mutate(ZIP = sapply(strsplit(df$X4, split='|', fixed=TRUE), function(x) (x[2]))) 

df <- df %>%
  mutate(DATA = sapply(strsplit(df$ZIP, split=c(','), fixed=TRUE), function(x) (x[2]))) 

df <- df %>%
  mutate(ZIP = sapply(strsplit(df$DATA, split=c(' '), fixed=TRUE), function(x) (x[2]))) 


df<-df %>% 
  mutate(ZIP=as.numeric(ZIP))

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
  theme(panel.background = element_blank()) + ggtitle("US Dunkin' Donut's Location", subtitle = "Average # of Stores") + labs(fill = "Average") + theme(legend.position="bottom", plot.title = element_text(hjust = 0.5),plot.subtitle =  element_text(hjust = 0.5))+
  scale_fill_gradient(low = "orange", high = "purple", guide = guide_legend()) + geom_text(data = centroid, mapping = aes(x_avg,y_avg, group = group, label = n), color = "black", inherit.aes = FALSE)

write_csv(data, "data/DunkinAvgLocations#1.csv")
write_csv(final_data, "data/DunkinAvgLocations#2.csv")
#---------------------------------------------------------------------------------------------------------------------------
# by county

df<- read_csv("data/DD-US.csv",col_names = FALSE)
zip<-read_csv("data/ZIP-COUNTY-FIPS-2018.csv")

cmap<-county_map
us_states<-map_data("state")

zip<-zip %>% 
  mutate(ZIP=as.numeric(ZIP))

df<-df %>% 
  mutate(ZIP=as.numeric(str_match(X4,"(\\d{5})")[1]))

data<-left_join(zip,df,by="ZIP") %>% 
  mutate(COUNTYNAME=str_remove(COUNTYNAME," County")) %>% 
  rename(county=COUNTYNAME) %>% 
  mutate(county=tolower(county)) %>% 
  group_by(county, STATE) %>% 
  count() %>% 
  na.omit() 

data <- data %>% 
  summarise(s= max(n)) %>% 
  arrange(desc(s)) %>% 
  na.omit() 

r <- us_map(regions = "counties")

r <- r %>% 
  rename(
    STATE = abbr
  )
r <-  r %>% 
  mutate(county = str_remove(county, " County"))

r <- r %>% 
  mutate(county = tolower(county))

r <- r %>% 
  group_by(STATE, county) 
 
data<-left_join(r,data, by = c("STATE","county"))

new_data <- data %>% 
  select(STATE, county, s) %>% 
  group_by(STATE,county) %>% 
  summarise(count= max(s)) %>% 
  arrange(desc(count)) %>% 
  na.omit() %>% 
  head(20) 

write_csv(new_data, "data/DunkinAvgLocations#3.csv")
