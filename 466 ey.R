getwd()
setwd("D:/IST 421")
library(ggplot2)
library(readr)
library(dplyr)
library(RColorBrewer)
library(tidyr)
library(viridis)
library(hrbrthemes)
flights<-read_csv("2.csv")
is.na(flights)
flights<-flights[-c(1,2,3),]
colnames(flights)
newcol<-c("Airport","City","Month","Carrier","No_of_Flights")
colnames(flights)<-newcol
colnames(flights)


unique(flights$Carrier)
flights$Carrier[flights$Carrier=="14F"]<-"American Airlines"
flights$Carrier[flights$Carrier=="56Q"]<-"United Airlines"
flights$Carrier[flights$Carrier=="WN3"]<-"Southwest Airlines"

american<-select(filter(flights,Carrier== "American Airlines"),c(colnames(flights)))
united<-select(filter(flights,Carrier== "United Airlines"),c(colnames(flights)))
southwest<-select(filter(flights,Carrier== "Southwest Airlines"),c(colnames(flights)))


flights$Month<-as.numeric(flights$Month)

flights$No_of_Flights<-as.numeric(flights$No_of_Flights)
sum(is.na(flights$No_of_Flights))
mean(flights$No_of_Flights,na.rm = T)
flights$No_of_Flights[is.na(flights$No_of_Flights)]=0


ggplot(flights,aes(Carrier,No_of_Flights,fill=Carrier,label=No_of_Flights))+
  geom_bar(stat="identity", position="dodge")+
  labs(title="The Distribution of Number of Flights among 3 Carriers")+
  ylab("Number of Flights")

no_flights<-tapply(flights$No_of_Flights,list(flights$Airport),sum)
no_flights
ggplot(flights,aes(Airport,no_flights))
airport<-unique(flights$Airport)
a1<-data.frame(rbind(airport,no_flights))
a1

ggplot(flights,aes(No_of_Flights,fill=Airport))+
  geom_density(adjust=1.5)+
  scale_fill_brewer(palette="Accent")+
  theme_ipsum()+
  labs(title = "Density Plot of Number of Flights")

ggplot(flights,aes(No_of_Flights,fill=Airport))+
  geom_density(adjust=1.5)+
  scale_fill_brewer(palette="Accent")+
  theme_ipsum()+
  facet_wrap(Airport~.)+
  labs(title = "Density Plot of Number of Flights")


flights%>% group_by(Airport)%>%
  summarize(Total_No_of_Flights = sum(No_of_Flights))%>%
  ggplot(aes(x=Airport,y=Total_No_of_Flights))+
  scale_fill_brewer(palette="Accent")+
  geom_col()+
  labs(title = "Distribution of Number of Flights in Airports")

arrange(flights,Month)
flights%>% group_by(Month)%>%
  summarize(Total_No_of_Flights = sum(No_of_Flights))%>%
  ggplot(.,aes(x=Month,y=Total_No_of_Flights))+
  geom_line(color = '#E51837', size = .6)+
  geom_point()+
  labs(title = " The Trend in Overall Number of Flights Over the Last 6 Months of the Year 2017")
  
covid<-select(filter(flights,Month>=7 & Month<= 9),colnames(flights))%>%
  arrange(Month)
covid%>%group_by(City)%>%
  summarise(Total_No_of_Flights=sum(No_of_Flights))%>%
  mutate(Total_No_of_Flights=replace(Total_No_of_Flights,which(City=="New York"),19787*.2))%>%
  mutate(Total_No_of_Flights=replace(Total_No_of_Flights,which(City=="Los Angeles"),28294*.4))%>%
  mutate(Total_No_of_Flights=replace(Total_No_of_Flights,which(City=="Seattle"),9269*.5))
  
  
  

delay<-read_csv("1.csv")
delay
