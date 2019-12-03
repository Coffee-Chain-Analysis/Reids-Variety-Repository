library(tidyverse)
library(cowplot)
library(socviz)
library(usmap)
library(RColorBrewer)

dunk_rev<- read_csv("data/dunkin.csv")

dunk_rev <- dunk_rev %>% 
  select(year_of_publish, DD_US_F:BR_US_C, Franchise_income, Rental_income:Total_reveneu)

dunk_rev <- dunk_rev %>% 
  rename(
    `Dunkin US Franchise` = DD_US_F,
    `Dunkin International Franchise` = DD_IN_F,
    `Baskin Robin US Franchise` = BR_US_F,
    `Baskin Robin International Franchise` = BR_IN_F,
    `Baskin Robin US Company` = BR_US_C,
    `Dunkin US Company` = DD_US_C
  )
dunk_rev <- dunk_rev %>% 
  select(`Dunkin US Franchise`, `Dunkin International Franchise`, `Baskin Robin US Franchise`,  `Baskin Robin International Franchise`,`Baskin Robin US Company`,`Dunkin US Company`, year_of_publish) %>% 
  gather(Type, count,`Dunkin US Franchise`, `Dunkin International Franchise` , `Baskin Robin US Franchise` ,`Baskin Robin International Franchise`,`Baskin Robin US Company`,`Dunkin US Company`) %>% 
  na.omit()



dunk_rev <- dunk_rev%>% 
  group_by(Type, year_of_publish) %>% 
  summarise(mean = mean(count))

dunk_rev %>% 
    ggplot() +
    geom_col(mapping = aes(reorder(year_of_publish, mean),
                           y = mean, fill=Type)) +
    theme(plot.title = element_text(hjust = 0.5)) + ylab("Store Count") + xlab("Year")+ labs(title = "Dunkin Donut's Store Count")+ facet_wrap(~Type, scales = "free")

# dunk_rev %>%
#   group_by(Type) %>% 
#   ggplot(mapping = aes(x = year_of_publish, y = mean)) +
#   geom_line(mapping = aes(color = Type)) + ylab("Store Count") + xlab("Year")+ facet_wrap(~Type, scales = "free")+ labs(title = "Dunkin Donut's Store Count")+ theme(plot.title = element_text(hjust = 0.5))

write_csv(dunk_rev, "data/dunkinrevenue#1.csv")

#----------------------------------------------------------------------------
dunk_rev<- read_csv("data/dunkin.csv")

dunk_rev <- dunk_rev %>% 
  select(year_of_publish, DD_US_F:BR_IN_F, Franchise_income, Rental_income:Total_reveneu)

dunk_rev <- dunk_rev %>% 
  rename(
    `Franchise Income` = Franchise_income,
    `Rental Income` = Rental_income,
    `Sales of Company Restaruant` = `Sales_of_company restaurant`,
    `Sales of IceCream` = Sales_of_Icecream,
    `Total Revenue` = Total_reveneu,
    `Other Revenue` = Other_reveneue
 
  )
dunk_rev <- dunk_rev %>% 
  select( `Franchise Income` , `Rental Income`, `Sales of Company Restaruant`, `Sales of IceCream`,`Total Revenue`, `Other Revenue`, year_of_publish) %>% 
  gather(Type, count, `Franchise Income`, `Rental Income`, `Sales of Company Restaruant`, `Sales of IceCream`,`Total Revenue`,`Other Revenue`) %>% 
  na.omit()


dunk_rev <- dunk_rev%>% 
  group_by(Type, year_of_publish) %>% 
  summarise(mean = mean(count))

dunk_rev %>%
  group_by(Type) %>% 
  ggplot(mapping = aes(x = year_of_publish, y = mean)) +
  geom_line(mapping = aes(color = Type)) + ylab("Revenue") + xlab("Year")+ facet_wrap(~Type, scales = "free")+ theme(plot.title = element_text(hjust = 0.5))+ labs(title = "Dunkin Donut's Revenue")


write_csv(dunk_rev, "data/dunkrevenue#2.csv")

