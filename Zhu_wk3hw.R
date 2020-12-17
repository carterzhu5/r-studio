getwd()
setwd("D:/IST 421")


art<-read.csv(file="art.csv",
              header = T,
              stringsAsFactors = F)
par(mfrow = c(2,2))

#question 1:Is there a relationship between the unit price of art goods and their units sold? If so, what kind of relationship is it?
str(art)
boxplot(art$units.sold~art$unit.price,
        main="relationship between unit price and units sold",
        col = "red",
        xlab = "unit price",
        ylab = "unis sold")

#Yes, there is a relationship, the number of units sold decreases as the unit price increases. 


#question 2:Does the art company sell more units of drawing paper or watercolor paper?

art$paper
sum(art$units.sold[art$paper=="watercolor"])
sum(art$units.sold[art$paper=="drawing"])

unit.by.paper<-aggregate(art$units.sold,
                         by = list(art$paper),
                         FUN=sum)
unit.by.paper

barplot(unit.by.paper[,2],names.arg = unit.by.paper[,1],col= c("red","green"),
        main = "units sold in paper types",
        xlab = "paper type",
        ylab = "units sold",
        space = 1)

# as the bar plot shows, it does not. More watercolor paper were sold than drawing paper.

#question 3: Does the art company bring in more money (revenue) selling drawing paper or watercolor paper? 
art$total.sale
sum(art$total.sale[art$paper=="watercolor"])
sum(art$total.sale[art$paper=="drawing"])

unit.by.total.sale<- aggregate(art$total.sale,
                               by = list(art$paper),
                               FUN=sum)

barplot(unit.by.total.sale[,2],names.arg = unit.by.total.sale[,1],
        col= c("blue","yellow"),
        main = "total sale in paper types",
        xlab = "paper type",
        ylab = "total sale",
        width = 10)

#the art company got more revenue on selling watercolor paper than selling drawing paper

#question 4:Each paper (watercolor and drawing) has different subtypes. It is possible that at some stores, some subtypes sell better than others. For drawing paper only, make a plot that allows the viewer to compare which subtypes of drawing paper sell more (and less) units across the stores.

art_drawing<- subset(art,paper=="drawing",
                     select = c("store","paper.type","units.sold"))

units.subtype.store.sale<-tapply(art_drawing$units.sold,
                                 list(art_drawing$store,art_drawing$paper.type),
                                 sum)

units.subtype.store.sale

barplot(units.subtype.store.sale,beside = T,
        legend.text =row.names(units.subtype.store.sale),
        main = "units sold in subtypes of drawing paper",
        xlab = "subtypes",
        ylab = "units sold")

#question 5: The dataset covers 4 years of data. Compare the revenue gained each year from the sales drawing paper with that of watercolor paper. Are sales growing over time for both?

r<-tapply(art$total.sale,list(art$paper,art$year),sum)

x<-as.numeric(colnames(r))
options(scipen = 999)

plot(x,r[1,],type="l",col="light blue",lwd=2,
    ylab = "total sales",
    xlab = "years",
    main = "total sales in years",
    ylim = c(0,max(r)),bty="n")

lines(x,r[2,],col="red",lwd=2)

legend('bottomleft',legend =rownames(r),lwd = 2,lty=1,
       col = c("lightblue","red"),bty = 'n',cex = .75)
