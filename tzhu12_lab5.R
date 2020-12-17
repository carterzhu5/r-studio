setwd("D:/IST 421")
getwd()
sales<- read.csv("sales.csv",
                 header = T,
                 stringsAsFactors = F)
par(mfrow=c(1,1))
library(RColorBrewer)
display.brewer.all()

rand.data<-replicate(9,rnorm(35,35,sd=1.5))
boxplot(rand.data,col=brewer.pal(8,"Set3"))
brewer.pal(8,"Set3")

num.colors<-8
FUN<-colorRampPalette(c("blue","red"))
my.cols<-FUN(num.colors)
boxplot(rand.data,col=my.cols)

FUN(16)

plot(sales$expenses,sales$recipt,pch=16,cex=1)

col.vec<-rep(rgb(30,144,255,maxColorValue = 255),dim(sales)[1])
col.vec[sales$rep.sex==0]<-rgb(255,64,64,maxColorValue = 255)

plot(sales$expenses,sales$recipt,pch=16,cex=1,col=col.vec)

hist(sales$unit.price)

plot(sales$expenses,sales$recipt,pch=16,cex=1.5)

over.plotting.cols<-rgb(.8,.15,.15)
plot(sales$expenses,sales$recipt,pch=16,cex=1.5,col=over.plotting.cols)

over.plotting.cols<-rgb(.8,.15,.15,alpha = .3)
plot(sales$expenses,sales$recipt,pch=16,cex=1.5,col=over.plotting.cols)

over.plotting.cols<-rgb(24,116,205,alpha = 75,maxColorValue = 255)
plot(sales$expenses,sales$recipt,pch=16,cex=1.5,col=over.plotting.cols)


barplot(table(sales$wine))

agg.data<-aggregate(sales$units.sold,by=list(type=sales$type,wine=sales$wine),
                                             FUN=sum)
barplot(agg.data$x,names.arg = agg.data$wine)                    

wine.colors<-c(rgb(255,240,150,maxColorValue = 255),
              rgb(160,30,65,maxColorValue = 255))
pie(c(10,10),col = wine.colors)

bar.colors<-rep("white",nrow(agg.data))
bar.colors[agg.data$type=="white"]<-wine.colors[1]
bar.colors[agg.data$type=="red"]<-wine.colors[2]
barplot(agg.data$x,names.arg = agg.data$wine,col = bar.colors)

barplot(agg.data$x,names.arg = agg.data$wine,col = bar.colors,border = NA,main = "units sold")

agg.data.receipts<-aggregate(sales$recipt,by=list(type=sales$type,
                                                  wine=sales$wine),
                             FUN=sum)

options(scipen = 999)
colnames(agg.data.receipts)[3]<-"units"

agg.data$receipts<-agg.data.receipts[,3]

colnames(agg.data)[3]<-"units"
agg.data
library(png)

ima<-readPNG("bottles.png")
r1<-readPNG("r1.png")
w1<-readPNG("w1.png")

pch<-rep("w",7)
pch[agg.data$type=="red"]<-"R"
plot(agg.data$units,agg.data$receipts,bty="n",
     pch=15, cex=2, col=bar.colors,
     xlim = c(0,1.25* max(agg.data$units)),
     ylim = c(0,1.25* max(agg.data$receipts)),
     xlab =  "Units sold",ylab = "Receipts",
     main = "IST 421 Simulated wine data")

lim<-par()

rasterImage(ima,lim$usr[1],lim$usr[3],lim$usr[2],lim$usr[4])
rect(lim$usr[1],lim$usr[3],lim$usr[2],lim$usr[4],col = rgb(1,1,1,.75),border = "white")

r1.x1<-agg.data$units[agg.data$type=="red"]
r1.x2<-r1.x1+3000
r1.y1<-agg.data$receipts[agg.data$type=="red"]
r1.y2<-r1.y1+65000

rasterImage(r1,r1.x1,r1.y1,r1.x2,r1.y2)

text(agg.data$units+2000, agg.data$receipts, labels = agg.data$wine,adj=0,cex=1.2)


#ggplot
install.packages("tidyverse")
library(tidyverse)

#count rows

ggplot(sales)+geom_bar(aes(x=wine))

#plot aggreagated units sold

ggplot(agg.data)+geom_col(aes(x=wine,y=units))

#controlling color
ggplot(agg.data)+geom_col(aes(x=wine,y=units,fill=type))+
  scale_fill_manual(values=wine.colors)

names(wine.colors)<-c("white","red")

ggplot(agg.data)+geom_col(aes(x=wine,y=units,fill=type))+
  scale_fill_manual(values=wine.colors)

#theming

ggplot(agg.data)+geom_col(aes(x=wine,y=units,fill=type))+
  scale_fill_manual(values=wine.colors)+theme_minimal()

ggplot(agg.data)+geom_col(aes(x=wine,y=units,fill=type))+
  scale_fill_manual(values=wine.colors)+theme_minimal()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 45,hjust = 1))

aggregate(sales$units.sold,by=list(sales$rep.region,sales$type),FUN=sum)

a1<-sales %>%
  group_by(rep.region,type)%>%
  summarise(units=sum(units.sold))
a1

ggplot(a1)+
  geom_col(aes(x=rep.region,y=units,fill=type),position="dodge")+
  ggtitle("Wine type by region")+
  xlab("region")



#Write a piece of R code to find the names of the sales representatives that 
#sold the most units of white wine in each region in 2010. (sales.csv)



sales2010<-subset(sales, year==2010 &type=="white")
list2010=tapply(sales2010$units.sold, list(sales.rep=sales2010$sales.rep,sales2010$rep.region),FUN=sum)
best.rep<-as.data.frame(list2010)
#Central region
rownames(best.rep[which.max(best.rep$Central),])
#East region
rownames(best.rep[which.max(best.rep$East),])
#North region
rownames(best.rep[which.max(best.rep$North),])
#South region
rownames(best.rep[which.max(best.rep$South),])
#West region
rownames(best.rep[which.max(best.rep$West),])

#Write the code necessary to find out which wine is responsible for the highest recipts in 2012 for each region? (sales.csv)


sales_2012<-subset(sales, year==2012)
rece.2012<-tapply(sales_2012$recipt, list(sales_2012$wine,sales_2012$rep.region),FUN=sum)
high.rec<-as.data.frame(rece.2012)

rownames(high.rec[which.max(high.rec$Central),])

rownames(high.rec[which.max(high.rec$East),])

rownames(high.rec[which.max(high.rec$North),])

rownames(high.rec[which.max(high.rec$South),])

rownames(high.rec[which.max(high.rec$West),])

