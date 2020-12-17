dev.off()
cat('\014')  
rm(list=ls()) 
library(tidyverse)
storm_data<-read_csv("StormEvents.gz",col_types = cols(EPISODE_NARRATIVE="c",EVENT_NARRATIVE="c",WFO='c',EPISODE_ID='c',TOR_OTHER_CZ_STATE='c',SOURCE='c',BEGIN_LOCATION='c',END_LOCATION='c',TOR_OTHER_CZ_FIPS='c',TOR_OTHER_CZ_NAME='c',BEGIN_AZIMUTH='c',END_AZIMUTH='c',DAMAGE_PROPERTY="c",DAMAGE_CROPS='c',MAGNITUDE_TYPE='c',FLOOD_CAUSE='c',TOR_OTHER_WFO='c',CATEGORY='i'))

View(storm_data)

library(dplyr)
state_count <- aggregate(storm_data$EVENT_TYPE,by=list(storm_data$STATE),FUN=length)
storm_type_count <- count(storm_data, EVENT_TYPE)
storm_type_count
storm_type_count %>% arrange(desc(n))

library(ggplot2)
storm_type_count %>% arrange(desc(n)) %>%
  top_n(storm_type_count, 10) %>% ggplot(storm_type_count)

storm_type_count<-data.frame(count(storm_data,EVENT_TYPE))
storm_type_count
storm_type_count%>% top_n(10)%>%
  arrange(desc(n))%>%
  ggplot(.,aes(x=reorder(EVENT_TYPE,n),y=n))+
  geom_col()

library(maps)
map("state")

agg.data <- storm_data %>% group_by(STATE) %>% summarise(deaths = sum(DEATHS_DIRECT))
agg.data
agg.data2 <- storm_data %>% group_by(STATE) %>% summarise(injuries = sum(INJURIES_DIRECT))
agg.data2

m <- maps::map("state")
state.order <- match.map(database = "state", regions = agg.data$STATE, exact = F, warn = T)
cbind(m$names, agg.data[state.order,]$STATE, agg.data[state.order,]$deaths)

num.cols <- 10
library(viridis)
my.color.vec <- viridis(num.cols, option = "inferno")
library(plotrix)
index <- rescale(x = agg.data$deaths, c(1,num.cols))
index <- round(index)
index
agg.data$col <- my.color.vec[index]
agg.data
library(mapproj)
maps::map("state", col = agg.data$col[state.order], fill = T, resolution = 0, lty = 1, 
          projection = "polyconic", border = "tan")
mtext(text = "Deaths from Storms by State", side = 3, line = 2, cex = 2)

library(usmap)
library(rgdal)
names(agg.data)[names(agg.data) == "STATE"] <- "state"
names(agg.data2)[names(agg.data2) == "STATE"] <- "state"
plot_usmap(data = agg.data, values = "deaths", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Deaths from Storms")+
  theme(legend.position = "right")+
  labs(title = "Deaths from Storms By State", cex = 2)
plot_usmap(data = agg.data2, values = "injuries", color = "black")+
  scale_fill_continuous(name = "Injuries by State")+
  theme(legend.position = "right")+
  labs(title = "Injuries from Storms By State")

state_count <- state_count %>% arrange(desc(x))
state_count <- state_count %>% top_n(10)
state_count <- state_count %>% as.numerical(state_count$x)'
state_count
barplot(state_count$x, names.arg = state_count$Group.1, col = "firebrick1",
        xlab = "States",
        ylab = "Number of Storms",
        main = "Top 10 States with the Most Storms")

texas <- storm_data[storm_data$STATE == "TEXAS",]
texas_count<-data.frame(count(texas,EVENT_TYPE))
texas_count
texas_count%>% top_n(10)%>%
  arrange(desc(n))%>%
  ggplot(.,aes(x=reorder(EVENT_TYPE,n),y=n))+
  geom_col()

