library(tidyverse)
library(cowplot)
library(socviz)
library(usmap)
library(RColorBrewer)

#Revenue of company stores
starbuck_rev<- read_csv("data/starbucks.csv")

starbuck_rev <- starbuck_rev %>% 
  select(year, Company_Operated_Stores:Total_OS)

starbuck_rev <- starbuck_rev %>% 
  select(Company_Operated_Stores, License_Stores, Other, Total,year) %>% 
  gather(Type, count, Company_Operated_Stores, License_Stores, Other, Total) %>% 
  na.omit()

starbuck_rev <- starbuck_rev %>% 
  group_by(Type, year) %>% 
  summarise(mean = mean(count))
# starbuck_rev %>% 
#   ggplot() + 
#   geom_col(mapping = aes(reorder(year, mean),
#                          y = mean, fill=Type)) + labs(title = "Starbuck's Revenue")+
#   theme(plot.title = element_text(hjust = 0.5)) + xlab("Year") + ylab("Revenue")  +  facet_wrap(~Type, nrow = 3, scales = "free") + coord_flip()

starbuck_rev %>%
group_by(Type) %>%
  ggplot(mapping = aes(x = year, y = mean)) +
  geom_line(mapping = aes(color = Type)) + ylab("Revenue") + xlab("Year")+ facet_wrap(~Type, nrow = 3,scales = "free") + labs(title = "Starbuck's Revenue")+ theme(plot.title = element_text(hjust = 0.5))

write_csv(starbuck_rev, "data/TotalRevenue.csv")

#revenue by country
starbuck_rev<- read_csv("data/starbucks.csv")

starbuck_rev <- starbuck_rev %>% 
  select(year, Company_Operated_Stores:Total_OS)

starbuck_rev <- starbuck_rev %>% 
  select(Americas_Licensed, EMEA_Licensed, CP_Licensed, Total_LS, year) %>% 
  gather(Type, count, Americas_Licensed, EMEA_Licensed, CP_Licensed, Total_LS) %>% 
  na.omit()

starbuck_rev <- starbuck_rev %>% 
  group_by(Type, year) %>% 
  summarise(mean = mean(count))
# starbuck_rev %>% 
#   ggplot() + 
#   geom_col(mapping = aes(reorder(year, mean),
#                          y = mean, fill=Type)) + labs(title = "Starbuck's Revenue/Country(LS)")+
#   theme(plot.title = element_text(hjust = 0.5)) + xlab("Year") + ylab("Revenue") + scale_fill_brewer(palette = "Set2")  +  facet_wrap(~Type, nrow = 3, scales = "free") + coord_flip()
#   scale_y_continuous(limits = c(0, 20000))

starbuck_rev %>%
  group_by(Type) %>%
  ggplot(mapping = aes(x = year, y = mean)) +
  geom_line(mapping = aes(color = Type)) + ylab("Revenue") + xlab("Year")+ facet_wrap(~Type, nrow = 3,scales = "free")+ labs(title = "Starbuck's Revenue")+ labs(title = "Starbuck's Revenue/Country(LS)")+
theme(plot.title = element_text(hjust = 0.5))


write_csv(starbuck_rev, "data/revenue(Ls).csv")
#revenue by country(com)

starbuck_rev<- read_csv("data/starbucks.csv")

starbuck_rev <- starbuck_rev %>% 
  select(year, Company_Operated_Stores:Total_OS)

starbuck_rev <- starbuck_rev %>% 
  select(Americas_Com, EMEA_Com, CP_Com, Total_OS, year) %>% 
  gather(Type, count, Americas_Com, EMEA_Com, CP_Com, Total_OS) %>% 
  na.omit()

starbuck_rev <- starbuck_rev %>% 
  group_by(Type, year) %>% 
  summarise(mean = mean(count))

# starbuck_rev %>% 
#   ggplot() + 
#   geom_col(mapping = aes(reorder(year, mean),
#                          y = mean, fill=Type)) + labs(title = "Starbuck's Reveue/Country(OS)")+
#   theme(plot.title = element_text(hjust = 0.5)) + xlab("Year") + ylab("Revenue") + scale_fill_brewer(palette = "Set2")  +  facet_wrap(~Type, nrow = 3, scales = "free") + coord_flip()

starbuck_rev %>%
  group_by(Type) %>%
  ggplot(mapping = aes(x = year, y = mean, )) +
  geom_line(mapping = aes(color = Type)) + ylab("Revenue") + xlab("Year")+ facet_wrap(~Type, nrow = 3,scales = "free") + labs(title = "Starbuck's Reveue/Country(OS)") + theme(plot.title = element_text(hjust = 0.5))

write_csv(starbuck_rev, "data/revenue(Com).csv")
