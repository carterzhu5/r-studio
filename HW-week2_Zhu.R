getwd()
hotdog<- read.csv(file = "hot-dog-contest-winners.csv",
                  header = TRUE,
                  stringsAsFactors = F)

barplot(hotdog$Dogs.eaten, names.arg = hotdog$Year)

barplot(hotdog$Dogs.eaten, names.arg = hotdog$Year, col = "red",
        border = NA, xlab = "Year", ylab = "Hot dogs and buns (HDB)eaten")

fill_colors <- c()
for ( i in 1:length(hotdog$Country) ) {
  if (hotdog$Country[i] == "United States") {
    fill_colors <- c(fill_colors, "#821122")
  } else {
    fill_colors <- c(fill_colors, "#cccccc")
  }
}

barplot(hotdog$Dogs.eaten, names.arg = hotdog$Year, col = fill_colors,
        main= "Nathan's Hot Dog Eating Contest Results, 1980-2010", border = NA, space = 0.3, xlab = "Year", ylab = "Hot dogs and buns (HDB)eaten")

hot_dog_places <- read.csv(file= "hot-dog-places.csv",
            sep=",", header=TRUE,
            stringsAsFactors = F)
hot_dog_places

hot_dog_matrix<-as.matrix(hot_dog_places)

barplot(hot_dog_matrix,space=0.25, ylim = c(0,200),xlab = "Year",
        ylab = "Hot Dogs and Buns (HDB) eaten",
        main = "Hot Dog Eating Contest Results, 1980-2010")


subscribers<- read.csv(file="flowingdata_subscribers.csv",
                       header= TRUE,
                       stringsAsFactors = F)

plot(subscribers$Subscribers)
plot(subscribers$Subscribers, type = "p", ylim = c(0,30000))

plot(subscribers$Subscribers, type="h", ylim=c(0, 30000),
     xlab="Day", ylab="Subscribers")


points(subscribers$Subscribers, pch=19, col="black")

population<- read.csv(file = "world-population.csv",
                      header = TRUE,
                      stringsAsFactors = F)
plot(population$Year, population$Population, type="l",
     ylim=c(0, 7000000000), xlab="Year", ylab="Population")


postage<- read.csv(file="us-postage.csv",
                   header = TRUE,
                   stringsAsFactors = F)

plot(postage$Year,postage$Price, type = "s")
plot(postage$Year, postage$Price, type="s",
     main="US Postage Rates for Letters, First Ounce, 1991-2010",
     xlab="Year", ylab="Postage Rate (Dollars)")

art<- read.csv(file="art.csv",
               header=TRUE,
               stringsAsFactors = F)
par(mfrow=c(2,2))
plot(art$total.sale,main = "Distribution of totalsale",
     xlab = "position", ylab ="total sale",
     pch=8,bg="lightgray",col="blue",lwd=0.6)

hist(art$total.sale,main = "Distribution of totalsale",col="orange",border = "blue",
     xlab = "total sale",ylab = "numbers of artists")

art_drawing<-subset(art,paper=="drawing",select = c("total.sale"))
art_watercolor<-subset(art,paper=="watercolor",select = c("total.sale"))

boxplot(art_drawing$total.sale,main="the distribution of the totals sales for drawing paper",
        col = "green",xlab="drawing paper",ylab="total sales")

boxplot(art_watercolor$total.sale,main="the distribution of the totals sales for watercolor paper",
        col = "red",xlab="watercolor paper",ylab="total sales")   
