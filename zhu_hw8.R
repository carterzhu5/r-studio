getwd()
#The sales dataset has a number of wine sales people in the dataset. But to compare them we need to do groupings. 

sales<-read.csv("sales.csv",header = T, sep = ",")

recipts <- tapply(sales$recipt, list(sales$sales.rep), sum)
units <- tapply(sales$units.sold, list(sales$sales.rep), sum)
expenses <- tapply(sales$expenses, list(sales$sales.rep), sum)       
feedback <- tapply(sales$rep.feedback, list(sales$sales.rep), mean)

M1 <- rbind(recipts, units, expenses, feedback)
M2 <- cbind(recipts, units, expenses, feedback)
M1
M2
M1_matrix<-data.matrix(M1)
M1_hm<-heatmap(M1_matrix,Rowv = NA,
               Colv = NA,scale="row")

library(RColorBrewer)
display.brewer.all()

cols<-brewer.pal(9,"Blues")
M1_hm<-heatmap(M1_matrix,Rowv = NA,
               Colv = NA,scale="row",col=cols)

M1_hm<-heatmap(M1_matrix,Rowv = NA,
               Colv = NA,scale="row",col=cols,
          main="Heatmap of Sales")

install.packages("aplpack")
library(aplpack)
faces(M2)
ncolors
faces(M2,main = "Chernoff Faces of Sales",
      ncol.plot = 4,cex=1.5,plot.faces = T,face.type = 2)

stars(M2)
stars(M2, flip.labels = F)

stars(M2,flip.labels = F,full = F)
stars(M2,flip.labels = F,full = F,draw.segments = T)
stars(M2,flip.labels = F,full = F,draw.segments = T,
main = "Star Chart on Sales Performance")


stars(M2,flip.labels = F,full = F,draw.segments = T,
      main = "Star Chart on Sales")



library(lattice)
parallel(M2)
parallel(M2,horizontal.axis=F)
parallelplot(M2,horizontal.axis=F,col="#6666FF",
         main="Parallel Chart on Sales",
         lwd= 1.5,lty= 5)


