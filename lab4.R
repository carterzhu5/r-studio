getwd()
setwd("D:/IST 421")
getwd()

n<-1000
x<-rnorm(n)
y<-x^2+rnorm(length(x),mean = 1,sd=.25)
plot(c(x,-1.5,1.5,0),c(y,14,14,10),ylim=c(0,3),xlim=c(-1,1))

par(xpd=NA)
A<-sample(c("here","there","nowhere","everywhere"),size=n,prob=c(35,30,20,15),
          replace = T)

B<-sample(c("now","later"),size=n,prob = c(45,55),replace=T)
barplot(table(B,A),beside=T)
pie(table(A))
