setwd("D:/IST 421")
install.packages("tidyverse")
library(tidyverse)
install.packages("igraph")
library(igraph)

class.data <- read_csv("SocialNetwork.csv")
view(class.data)
node.info <- read_csv("MetaData.csv")
node.info <- node.info %>% rename(Plan3=X6)
view(node.info)
colnames(class.data) <-  gsub("\\s","",colnames(class.data))
class.data <- class.data %>% mutate(X1=gsub("\\s","",X1))
node.info <- node.info %>% mutate(Name=gsub("\\s","",Name))

class(class.data)
dim(class.data)
dim(node.info)

M <- as.matrix(class.data[,-1])
rownames(M) <- class.data$X1
M[is.na(M)] <- 0

g <- graph_from_adjacency_matrix(M)
g <- simplify(g)

vcount(g)
ecount(g)

V(g)$name
plot.igraph(g)
par(mar=c(1,1,1,1))
plot.igraph(g,edge.arrow.size=0,
            edge.arrow.width=0,
            main="IST421")

help("igraph.plotting")

par(mar=c(5,10,1,2))
plot(sort(degree(g)), cex.axis=.5)

barplot(sort(degree(g)),horiz = T,las=2,
        border = NA, col="darkgreen",
        main = "most freinds", space = 1,
        cex.names = .5)

barplot(sort(degree(g)),horiz = T,las=2,
        border = NA, col="darkgreen",
        main = "most freinds", space = 1,
        cex.names = .3)

barplot(sort(degree(g,mode = "in")),horiz = T,las=2,
        border = NA, col="darkgreen",
        main = "most popular", space = 1,
        cex.names = .4)

degs <- degree(g)
data <- tibble(name=names(degs), value=as.numeric(degs))
data$indegree <- as.numeric(degree(g,mode = "in"))
data$outdegree <- as.numeric(degree(g,mode = "out"))

ggplot(data)+geom_col(aes(name,value))+ggtitle("Friends")+coord_flip()


#Sort by degree
data <- data %>% arrange(value) %>% mutate(name=factor(name,name))
ggplot(data)+geom_col(aes(name,value))+ggtitle("Friends")+coord_flip()

data <- data %>% arrange(indegree) %>% mutate(name=factor(name,name))
ggplot(data)+geom_col(aes(name,indegree))+ggtitle("Popular")+coord_flip()

data <- data %>% arrange(outdegree) %>% mutate(name=factor(name,name))
ggplot(data)+geom_col(aes(name,outdegree))+ggtitle("Friendliest")+coord_flip()

ggplot(data)+geom_col(aes(name,outdegree))+
  ggtitle("Friendliest")+coord_flip()+
  theme(axis.text.y = element_text(size=5))


data.1 <- data %>% rename(degree=value) %>% gather(key = "measure",
                                                   value = "weight",
                                                   -name)
view(data.1)

ggplot(data.1)+geom_col(aes(name,weight))+
  ggtitle("Comparing measures")+coord_flip()+
  facet_wrap(~measure,nrow = 1)+
  theme(axis.text.y = element_text(size=5))

view(tibble(info=node.info$Name,node=V(g)$name))

V(g)$Program <- node.info$Program
V(g)$Plan <- node.info$Plan
V(g)$Plan3 <- node.info$Plan3
V(g)$Plan2 <- node.info$Plan2
V(g)$Level <- node.info$Level

View(igraph::as_data_frame(g,what = "vertices"))



install.packages("RColorBrewer")
library(RColorBrewer)
programs <- unique(node.info$Program)
programs
colors <- brewer.pal(length(programs), "Set3")
V(g)$color <- colors[match(V(g)$Program,programs)]
plot.igraph(g,edge.arrow.size=0,edge.arrow.width=0,main="Program Viz")
plot.igraph(g,edge.arrow.size=0,edge.arrow.width=0,main="Program Viz",
            vertex.label=NA)

# Only A&S people
df <- igraph::as_data_frame(g,what = "vertices") %>%
  mutate(name=ifelse(Program=="Arts and Sciences",name,NA))
asnames <- df$name
plot.igraph(g,edge.arrow.size=0,edge.arrow.width=0,main="Program Viz",
            vertex.label=asnames)
legend(x=.5, y=-.7, legend=programs, fill=colors)

# Look at levels
levels <- unique(node.info$Level)
levels
colors <- brewer.pal(length(levels), "Set3")
V(g)$color <- colors[match(V(g)$Level,levels)]
plot.igraph(g,edge.arrow.size=0,edge.arrow.width=0,main="Level Viz",
            vertex.label=NA)
legend(x=.5, y=-.7, legend=levels, fill=colors)

# Sizing the nodes
deg.size <- degree(g, mode="in")
plot(sort(deg.size), type = "l")

V(g)$indegree <- degree(g, mode="in")
V(g)$size <- V(g)$indegree

plot.igraph(g,edge.arrow.size=0, edge.arrow.width=0, vertex.label=NA)

scaling <- tibble(indegree = sort(deg.size))
scaling <- scaling %>% mutate(idx = row_number(),
                              norm = indegree/max(indegree),
                              sqrt = sqrt(indegree),
                              log10 = log10(indegree),
                              sqr = indegree^2,
                              rt.5 = indegree^-5,
                              rt.10 = indegree^-10)

#scaling <- scaling %>% gather(key=type, value=val,norm:rt.10)
#ggplot(scaling)+geom_line(aes(idx,val))+facet_wrap(~type)

plot(scaling$log10, type = "l")

V(g)$size <- log10(V(g)$indegree) * 20

plot.igraph(g,edge.arrow.size=0, edge.arrow.width=0, vertex.label=NA)



g1 <- delete_vertices(g,"JoshIntrone")
plot.igraph(g1,edge.arrow.size=0, edge.arrow.width=0, vertex.label=NA)

programs <- unique(node.info$Program)
programs
colors <- brewer.pal(length(programs), "Set3")
V(g1)$color <- colors[match(V(g1)$Program,programs)]
plot.igraph(g1,edge.arrow.size=0, edge.arrow.width=0, vertex.label=NA)
plot.igraph(g1,edge.arrow.size=0, edge.arrow.width=0)

E(g)$color <- "gold"
V(g)$outdegree <- degree(g, mode="out")
tmp.name <- V(g)$name[which.max(V(g)$outdegree)]
E(g)[from(tmp.name)]$color <- "blue"
E(g)[to("JoshIntrone")]$color <- "red"
plot.igraph(g,edge.arrow.size=0, edge.arrow.width=0)

# Laying out igraph
l <- layout_on_grid(g)
l <- layout_with_gem(g)
l <- layout_as_star(g, center = "JoshIntrone")
l <- layout_in_circle(g)
l <- layout_with_dh(g)
l <- layout_with_kk(g)


V(g)$x<-l[,1]
V(g)$y<-l[,2]
V(g)$frame.color=NA
V(g)$label.cex<-.25+(V(g)$outdegree/max(V(g)$outdegree))
V(g)$label.color<-"black"
par(mar=c(1,1,1,1))

plot.igraph(g,edge.arrow.size=0,edge.arrow.width=0)

install.packages("rgl")
library(rgl)
coords<-layout_with_kk(g,dim = 3)
rglplot(g,layout=coords)


