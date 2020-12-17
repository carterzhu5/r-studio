getwd()
setwd("D:/IST 421")
getwd()
sales<-read.csv(file="sales.csv",header = T,stringsAsFactors = F)
str(sales)
summary(sales)

# QUESTION: What is the relationship between expenses and recipt?

sales$expenses[1:10]
sales$recipt[1:10]

plot(sales$expenses,sales$recipt,main="scatter")
#This is right
abline(lm(sales$recipt ~sales$expenses),col="red",lwd=3)
#This is wrong
abline(lm(sales$expenses~sales$recipt),col="green",lwd=3)
abline(h=400,col="blue")

abline(v=9,col="blue")
rug(x=sales$recipt,side=2,col = "orange")
rug(x=sales$expenses,side=1,col = "orange")

# QUESTION: What is the relationship between recipt and type?
str(sales)
par(mfrow=c(1,2))
boxplot(sales$recipt~sales$type,main="Recipts")
boxplot(sales$expenses~sales$type,main="expenses")




# QUESTION: Which region sells the most units?
sum(sales$units.sold[sales$rep.region=="East"])
sum(sales$units.sold[sales$rep.region=="West"])

unit.by.reg<- aggregate(sales$units.sold,
                        by=list(sales$rep.region),
                        FUN=sum)

unit.by.reg
barplot(unit.by.reg[,2],names.arg = unit.by.reg[,1],main = "Low Dimensional plot")


# QUESTION: How do the units sold of red vs white wine differ by region?

units.by.reg.type<-aggregate(sales$units.sold,
                             by=list(sales$rep.region,sales$type),
                             FUN=sum)
units.by.reg.type

units.by.reg.type<-tapply(sales$units.sold,
                          list(sales$rep.region,sales$type),
                          sum)
units.by.reg.type

barplot(units.by.reg.type,beside = T,legend.text=(rownames(units.by.reg.type)))

units.by.type.reg<-t(units.by.reg.type)

units.by.type.reg

barplot(units.by.type.reg,beside = T,legend.text=rownames(units.by.type.reg))

units.by.type.reg<-tapply(sales$units.sold,
                          list(sales$type,sales$rep.region),
                          sum)
units.by.type.reg

# QUESTION: are recipts growing over time for each region?

M<-tapply(sales$recipt,list(sales$rep.region,sales$year),sum)
plot(M[1,],type = "l")
x<-as.numeric(colnames(M))
options(scipen = 999)

plot(x,M[1,],type="l",col="red",lwd=2,
     ylab = "Recipts in dollars",
     xlab="years",
     ylim = c(0,max(M)),bty="n")

lines(x,M[2,],col="blue",lwd=2)
lines(x,M[3,],col="orange",lwd=2)
lines(x,M[4,],col="green",lwd=2)
lines(x,M[5,],col="brown",lwd=2)

legend('bottomleft',legend =rownames(M),lwd = 2,lty=1,
       col = c("red","blue","orange","green","brown"),bty = 'n',cex = .75)


str(sales)

#Parsing datas
my.date<-as.Date(strptime(sales$sale.date[sales$year == 2014],
                          "%m/%d/%Y"))
class(my.date)

x<-seq.Date(from=min(my.date),to=max(my.date),by="day")
y=rep(0,length(x))
table(my.date)
tmp.tab<-table(my.date)
y[match(as.Date(names(tmp.tab)),x)]<-as.numeric(tmp.tab)
plot(x,y,type = "l",col="red")



df<-aggregate(sales$recipt[sales$year==2014],list(sales$sale.date[sales$year==2014]),FUN=sum)
df$date<-as.Date(strptime(df$Group.1,"%m/%d/%Y"))
plot(df$date,df$x,type = "h")

