setwd("D:/IST 421")
c(10,15,7,20)
pie(c(10,15,7,20),main="How many employees per division?")

pie(c(10,15,7,20),main="How many employees per division?", labels=c(10,15,7,20),
    col=c("red","tan","brown","orange"))
    

plot(c(1,2,4,8,1,4))

plot(c(1,2,4,8,1,4),pch=8)

#turn this one in

plot(c(1,2,4,8,1,4),pch=c(2,4,5,8,20))

plot(1:18,1:18,pch=1:18)

n<-20
plot(1:n,1:n,pch=1:n)

#turn this one in
plot(1:n,1:n,pch=1:n,cex=2,col="orange")

my.bucket<- rnorm(n=10)
plot(my.bucket)
plot(my.bucket, type="l")
plot(my.bucket, type="l",lwd=3)
plot(my.bucket, type="l",lwd=3,col="orange")
plot(my.bucket, type="l",lwd=3,col="orange",main = "Net worth over time",xlab = "years",ylab = "Dollars")

plot(my.bucket,type = "h")
plot(my.bucket,type = "h",lwd=2)
plot(my.bucket,type = "h",lwd=2,lty=3)
plot(my.bucket,type = "h",lwd=6,col=c("red","orange"))
plot(my.bucket,type = "h",lwd=6,col=c("red","orange","brown"),bty="n",bg="gray")


par()
plot(my.bucket, type = "h",lwd=6,col=c("red","orange","brown"),bty="n",bg="gray")

par(bg="white")

plot(my.bucket,type = "h",lwd=6,col=c("red","orange","brown"),bty="n")

par()

plot(my.bucket,type = "h",lwd=6,col=c("red","orange","brown"),bty="n")
dev.off()

plot(my.bucket,type = "h",lwd=6,col=c("red","orange","brown"),bty="n")

par(bg="gray")
#turn this one in

plot(my.bucket,type = "h",lwd=20,col=c("red","orange","brown"),bty="n",lend=1)

plot(my.bucket,type = "h",lwd=20,col=c("red","orange","brown"),bty="n",lend=1)

n<-27

my.letters.1<- sample(letters[1:3],size = n,replace = T)
my.letters.1[14]
my.letters.1[20:25]

my.letters.table<-table(my.letters.1)
barplot(my.letters.table)
barplot(my.letters.table,col = c("brown","tan","orange"),names.arg = c("sales","ops","delivery"),
        main = "employees by department",
        border = "white",
        ylab = "employees",
        xlab = "department",
        horiz = T,
        density = 20,
        angle = c(22,70,120))

x<-rnorm(n=1000,mean=10,sd=1)
hist(x,main="what is the distribution of x?")

boxplot(x,horizontal = T)

x<-rlnorm(n=1000,meanlog = 1,sdlog = 1)
hist(x,main="what is the distribution of x?")
boxplot(x,horizontal = T)


my.letters <- sample(letters[7:9], size = 50, replace =T)
barplot(table(my.letters))