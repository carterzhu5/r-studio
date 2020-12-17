getwd()

#Scatterplot Matrix, Figure 6-9
crime <-
  read.csv('http://datasets.flowingdata.com/crimeRatesByState2005.csv',
            sep=",", header=TRUE)

plot(crime$murder, crime$burglary)

crime2 <- crime[crime$state != "District of Columbia",]
crime2 <- crime2[crime2$state != "United States",]

plot(crime2$murder, crime2$burglary)

plot(crime2$murder, crime2$burglary, xlim=c(0,10), ylim=c(0, 1200))

scatter.smooth(crime2$murder, crime2$burglary,
               xlim=c(0,10), ylim=c(0, 1200))

plot(crime2[,2:9])

pairs(crime2[,2:9], panel=panel.smooth)



#Bubble chart, Figure 6-15

symbols(crime$murder, crime$burglary, circles=crime$population)

radius <- sqrt( crime$population/ pi )
symbols(crime$murder, crime$burglary, circles=radius)

symbols(crime$murder, crime$burglary, circles=radius, inches=0.35,
        fg="white", bg="red", xlab="Murder Rate", ylab="Burglary Rate",xlim=c(0,10))


text(crime$murder,crime$burglary,crime$state,cex = 0.5)

#Histogram, Figure 6-24

birth <- read.csv("http://datasets.flowingdata.com/birth-rate.csv")
stem(birth$X2008)

hist(birth$X2008,col = "purple")


#Density Plot. Figure 6-32
birth2008 <- birth$X2008[!is.na(birth$X2008)]
d2008 <- density(birth2008)
d2008frame <- data.frame(d2008$x, d2008$y)
write.table(d2008frame, "birthdensity.txt", sep="\t")
write.table(d2008frame, "birthdensity.txt", sep=",", row.names=FALSE)
plot(d2008, type="n")
polygon(d2008, col="#821122", border="#cccccc")

library(ggplot2)
sales<-read_csv("sales.csv")

names(sales)
view(sales)

ggplot(subset(sales,year<2013),aes(x=year,y=units.sold,group=type,color=type))+
  geom_col()+
  facet_wrap(~rep.region)+
  labs(title = "Sales Unit change between 2010 and 2013")+
  scale_x_continuous(breaks = c(2010,2013))

ggplot(sales,aes(x=rep.feedback,y=recipt,color=wine))+
  geom_point()+
  facet_wrap(~wine)+
  labs(title = "The relationship between representatives' feedback and reciept in different wines")
