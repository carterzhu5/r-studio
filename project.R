getwd()
setwd("D:/IST 421")
getwd()

install.packages("readxl")
library(readxl)
sc<-read_excel("Stephen Curry Stats.xlsx")
str(sc)
sc<-na.omit(sc)


hist(sc$Minutes,main = "Distribution of Stephen Curry minutes per game")
hist(sc$Minutes,main = "Distribution of Stephen Curry minutes per game",xlab = "Minutes",ylab = "Number of Games")
hist(sc$Minutes,main = "Distribution of Stephen Curry minutes per game",
     xlab = "Minutes",ylab = "Number of Games",
     col="yellow",border = "blue",density = 500,
     ylim = c(0,250))

boxplot(sc$PTS,main="Distribution of Stephen Curry scores per game")
boxplot(sc$PTS,main="Distribution of Stephen Curry scores per game",
col = "yellow",ylab = "Points")

counts<-table(sc$Opponent)
barplot(counts,main = "Distribution of Stephen Curry's games against opponents")
barplot(counts,main = "Distribution of Stephen Curry's games against opponents",
        las=2,col = "yellow",border = "blue",ylab = "Number of Games")

d<-density(sc$`3 Points Succesful`)
plot(d)
plot(d,main = "Density of Stephen Curry's Successful 3-point shots",
     xlab = "Successful Shots")
polygon(d, col="yellow", border="blue")


library(ggplot2)
aggregate(sc$`Total 3 Points`,by=list(sc$Result,sc$Type),FUN=sum)
a<-sc%>%
  group_by(Result,Type)%>%
  summarise(units=sum(`Total 3 Points`))
a<-a[-c(5,10),]

ggplot(a)+
  geom_col(aes(x=Type,y=units,fill=Result),position = "dodge")+
  ggtitle("How Stephen Curry's 3-pointers Affect Result in the Playoffs")

ggplot(a)+
  geom_col(aes(x=Type,y=units,fill=Result),position = "dodge")+
  ggtitle("How Stephen Curry's 3-pointers Affect Result in the Playoffs")+
          xlab("Game Type")+
  ylab("Number of 3-pointers")

ggplot(a)+
  geom_col(aes(x=Type,y=units,fill=Result),position = "dodge",width = 0.9)+
  ggtitle("How Stephen Curry's 3-pointers Affect Result in the Playoffs")+
  xlab("Game Type")+
  ylab("Number of 3-pointers")+
  scale_size_area()+
  scale_fill_viridis_d()
  