library(tidyverse)

install.packages("maps")
library(maps)
x <- c(8.11,8.21,8.11,8.33,8.34,8.11,7.91,7.79,8.68,9.82,10.33,8.66,7.62,7.5,7.64,6.32,5.75,0,0.11,1.2,0.96,2.72,3.01,3.93,4.58,4.52,4.68,4.47,4.64,4.95,5.36,6.27,8.11)
y <- c(7.78,6.78,6.35,5.23,4.34,1.87,1,0.54,0.77,1.15,1.02,0.21,0,0.23,0.75,1.52,2.41,2.54,3.05,3.94,4.71,4.73,4.66,4.8,5.15,5.66,5.85,5.99,6.43,6.58,7.07,7.72,7.78)
map("state")
m<-map("state")
ggplot()+geom_polygon(aes(m$x,m$y),color="black")+theme_void()

load("shootings.Rda")
names(shootings)

sort(shootings$State)
shootings$State<-gsub("^\\s+|\\s+$","",shootings$State)

agg.dat<-shootings %>% group_by(State)%>% summarise(victims=sum(Total.Number.of.Victims))
num.cols<-10
mycolors<-heat.colors(num.cols)
pie(rep(1,num.cols),col = mycolors)
mycolors<-rev(mycolors)
pie(rep(1,num.cols),col = mycolors)

library(plotrix)
index<-rescale(x=agg.dat$victims,c(1,num.cols))
index<-round(index)
agg.dat$col<-mycolors[index]
install.packages("mapproj")
library(mapproj)
state.order<-match.map(database = "state",regions = agg.dat$State,exact = F,warn = T)
state.order

cbind(m$names,agg.dat$State[state.order],agg.dat$col[state.order])
map("state",col=agg.dat$col[state.order],fill = T,resolution = 0,lty=1,projection = "polyconic",border="tan")
mtext(text = "Vitimes of mass shootings by state",side=3,line=2,cex=2)

#ggplot
agg.dat<-shootings%>% group_by(State)%>% summarise(victims=sum(Total.Number.of.Victims))%>% 
  mutate(region=tolower(State))%>% mutate(region=gsub("^\\s+|\\s+$","",region))

states<-maps::map("state")
mapdata<-merge(states,agg.dat,sort=F,by-"region")


head(us.cities)
my.cols<-rep(rgb(1,.6,.2,.7),length(us.cities$name))
my.cols[us.cities$capital>0]<-rgb(.2,.6,1,.9)
maps::map("state")
points(us.cities$long,us.cities$lat,col=my.cols,pch=16,cex=rescale(us.cities$pop,c(.5,7)))

install.packages("rworldmap")
library(rworldmap)
countries<-read.delim("countries.csv",quote = "\"",header = T,sep = ";",stringsAsFactors = F)

range(countries$Life.expectancy)
missing.data<-which(countries$Life.expectancy==0.0)
missing.data
countries<-countries[-missing.data,]
missing.data<-0

num.cats<-10
iso3.codes<-tapply(countries$Country..en.,1:length(countries$Country..en.),rwmGetISO3)
df<-data.frame(country=iso3.codes,labels=countries$Country..en.,life=countries$Life.expectancy)
df.map<-joinCountryData2Map(df,joinCode = "ISO3",nameJoinColumn = "country")

library(viridis)
mapCountryData(df.map,
               nameColumnToPlot = "life",
               catMethod = c("pretty","fixedWidth","diverging","logFixedWidth","quantiles")[2],
               colourPalette = viridis(num.cats,option = "inferno"),
               oceanCol = "royalblue4",
               borderCol = "white",
               missingCountryCol = "peachpuff4",
               mapTitle = "LifeExpectancy")

#ggplot
world_map<-map_data("world")
ggplot(world_map,aes(x=long,y=lat,group=group))+geom_polygon(fill="lightgray",color="white")
