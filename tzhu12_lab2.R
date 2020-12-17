getwd()
setwd("D:/IST 421")
getwd()

tips<- read.csv(file = "tips.csv",
                header = TRUE,
                stringsAsFactors = F)

my.dir<- getwd()
sales.fname<-"wine.txt"
sales.file<-paste(my.dir,sales.fname,sep = "/")
sales<-read.table(sales.file,
                  header = TRUE,
                  sep = "\t",
                  stringsAsFactors = F)

load("shootings.Rda")
junk<-list(1,"My stff", c(1,2,3))
save(junk,file = "junk.rda")
rm(junk)
load("junk.rda")

#remove everything rm(list=ls())

colnames(tips)
fix(tips)

#[row,col]
tips[1:10,]
tips[,3]

dim(tips)[1]
str(tips)


tips$smoker
tips[,"total_bill"]
tips$total_bill[1]

plot(tips$total_bill,main = "distribution of total")
hist(tips$total_bill,main="distribution of total")

boxplot(tips$total_bill,main="distribution of total")

d<-density(tips$total_bill)
plot(d,main = "distribution of total")

polygon(d,col = "orange",border = "blue")

plot(d$x,d$y,type="l")

tips$sex
unique(tips$sex)

males.bills<-tips$total_bill[tips$sex=="Male"]
female.bills<-tips$total_bill[tips$sex=="Female"]
par(mfrow=c(1,2))
hist(males.bills, main="dudes")
hist(female.bills,main="gals")

par(mfrow=c(3,2))
plot(tips$total_bill)
plot(sort(tips$total_bill))
hist(tips$total_bill)
boxplot(tips$total_bill)
plot(d)
polygon(d,col = "orange",border="blue")
plot(d$x,d$y,type = "l")

