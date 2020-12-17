getwd()
setwd("D:/IST 421")
getwd()
hotdogs <-
  read.csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv",
           sep=",", header=TRUE)
barplot(hotdogs$Dogs.eaten)
barplot(hotdogs$Dogs.eaten, names.arg=hotdogs$Year)
barplot(hotdogs$Dogs.eaten, names.arg=hotdogs$Year, col="red",
        border=NA, xlab="Year", ylab="Hot dogs and buns (HDB) eaten")


fill_colors <- c()
for ( i in 1:length(hotdogs$New.record) ) {
  if (hotdogs$New.record[i] == 1) {
    fill_colors <- c(fill_colors, "#821122")
  } else {
    fill_colors <- c(fill_colors, "#cccccc")
  }
}
barplot(hotdogs$Dogs.eaten, names.arg=hotdogs$Year, col=fill_colors,
        border=NA, space=0.3,ylim=c(0,70),xlab="Year", ylab="Hot dogs and buns (HDB)
eaten",main="Nathan's Hot Dog Eating Contest Results, 1980-2010")


hot_dog_places <-
  read.csv("http://datasets.flowingdata.com/hot-dog-places.csv",
            sep=",", header=TRUE)
hot_dog_matrix <- as.matrix(hot_dog_places)
barplot(hot_dog_matrix, border=NA, space=0.25, ylim=c(0, 200),
        xlab="Year", ylab="Hot dogs and buns (HDBs) eaten",
        main="Hot Dog Eating Contest Results, 1980-2010")


subscribers <-
  read.csv("http://datasets.flowingdata.com/flowingdata_subscribers.csv",
           sep=",", header=TRUE)

plot(subscribers$Subscribers, type="p", ylim=c(0, 30000))
plot(subscribers$Subscribers, type="h", ylim=c(0, 30000),
     xlab="Day", ylab="Subscribers")
points(subscribers$Subscribers, pch=19, col="black")


population <-
  read.csv("http://datasets.flowingdata.com/world-population.csv",
           sep=",", header=TRUE)
plot(population$Year, population$Population, type="l",
     ylim=c(0, 7000000000), xlab="Year", ylab="Population")


postage <- read.csv("http://datasets.flowingdata.com/us-postage.csv", sep=",", header=TRUE)

plot(postage$Year, postage$Price, type="s",
     main="US Postage Rates for Letters, First Ounce, 1991-2010",
     xlab="Year", ylab="Postage Rate (Dollars)")


unemployment <-
  read.csv(
    "http://datasets.flowingdata.com/unemployment-rate-1948-2010.csv",
    sep=",")

unemployment[1:10,]

plot(1:length(unemployment$Value), unemployment$Value)

scatter.smooth(x=1:length(unemployment$Value),
               y=unemployment$Value, ylim=c(0,11), degree=2, col="#CCCCCC", span=0.5)
