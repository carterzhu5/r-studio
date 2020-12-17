library(tidyverse)
sales<-read_csv("art_sales.csv")
inventory<-read_csv("art.inventory.prices.csv")

sales %>% mutate(check=drawing*watercolor) %>%
  summarise(dupes=sum(check=0))

sales<-sales%>% gather(media,sale,drawing:watercolor)
sales

sales<-sales%>%
  inner_join(inventory,by=c("media"="paper","paper.type"= "paper.type"))
sales

sales<-sales %>% mutate(units_sold=sale/unit.price)
sales
