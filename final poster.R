getwd()
setwd("D:/IST 421")

library(readxl)
library(ggplot2)
library(readr)
library(dplyr)
library(RColorBrewer)
library(tidyr)
library(viridis)
library(hrbrthemes)
library(grid)
library(gridExtra)
library(png)
library(RCurl)
library(tidyverse)
library(hexbin)
library(treemap)
library(scales)
library(ggthemes)
library(treemapify)

GSW<-read_csv("GSW.csv")
GSW$Team<-rep("GSW",nrow(GSW))
CLE<-read_csv("CLE.csv")
CLE$Team<-rep("CLE",nrow(CLE))
shotlog<-rbind(GSW,CLE)
shotlog<-na.omit(shotlog)
shotlog<-shotlog[,-c(1,5,14)]
shotlog <- subset(shotlog,position!="G")
colnames(shotlog)<-c("position","homeornot","x","hometeam","shot_type","points","awayteam","y","time","date","player","quarter","outcome","team")
shotlog$quarter[shotlog$quarter==5] <- 4
shotlog$quarter[shotlog$quarter==6] <- 4


shotlog$outcome[shotlog$outcome=="BLOCKED"] <- "MISSED"

shotlog$position[shotlog$position=="G"] <- "SG"

shotlog$shot_type[shotlog$shot_type=="Jump Shot"]<-"Catch&Shoot"
shotlog$shot_type[shotlog$shot_type=="Turnaround Fadeaway"]<-"Drving Pullup"
shotlog$shot_type[shotlog$shot_type=="Step Back Jump Shot"]<-"Drving Pullup"
shotlog$shot_type[shotlog$shot_type=="Pullup Jump Shot"]<-"Catch&Shoot"
shotlog$shot_type[shotlog$shot_type=="Turnaround Fadeaway Shot"]<-"Drving Pullup"
shotlog$shot_type[shotlog$shot_type=="Turnaround Jump Shot"]<-"Drving Pullup"
shotlog$shot_type[shotlog$shot_type=="Fadeaway Jumper"]<-"Drving Pullup"
shotlog$shot_type[shotlog$shot_type=="Running Pull-Up Jump Shot"]<-"Drving Pullup"
shotlog$shot_type[shotlog$shot_type=="Driving Floating Jump Shot"]<-"Floater"
shotlog$shot_type[shotlog$shot_type=="Running Jump Shot"]<-"Drving Pullup"
shotlog$shot_type[shotlog$shot_type=="Floating Jump Shot"]<-"Floater"
shotlog$shot_type[shotlog$shot_type=="Bank Shot"]<-"Catch&Shoot"


shotlog$shot_type[shotlog$shot_type=="Running Reverse Layup"]<-"Driving Layup"
shotlog$shot_type[shotlog$shot_type=="Cutting Layup Shot"]<-"Off-ball Layup"
shotlog$shot_type[shotlog$shot_type=="Running Layup"]<-"Driving Layup"
shotlog$shot_type[shotlog$shot_type=="Driving Layup"]<-"Driving Layup"
shotlog$shot_type[shotlog$shot_type=="Putback Layup"]<-"Off-ball Layup"
shotlog$shot_type[shotlog$shot_type=="Reverse Layup"]<-"Off-ball Layup"
shotlog$shot_type[shotlog$shot_type=="Tip Layup Shot"]<-"Off-ball Layup"
shotlog$shot_type[shotlog$shot_type=="Driving Reverse Layup"]<-"Driving Layup"
shotlog$shot_type[shotlog$shot_type=="Cutting Finger Roll Layup Shot"]<-"Off-ball Layup"
shotlog$shot_type[shotlog$shot_type=="Driving Finger Roll Layup"]<-"Driving Layup"
shotlog$shot_type[shotlog$shot_type=="Alley Oop Layup"]<-"Off-ball Layup"
shotlog$shot_type[shotlog$shot_type=="Finger Roll Layup"]<-"Driving Layup"
shotlog$shot_type[shotlog$shot_type=="Running Finger Roll Layup"]<-"Driving Layup"
shotlog$shot_type[shotlog$shot_type=="Running Finger Roll Layup"]<-"Driving Layup"
shotlog$shot_type[shotlog$shot_type=="Running Alley Oop Layup Shot"]<-"Driving Layup"

shotlog$shot_type[shotlog$shot_type=="Running Finger Roll Layup"]<-"Dunk"
shotlog$shot_type[shotlog$shot_type=="Driving Dunk"]<-"Dunk"
shotlog$shot_type[shotlog$shot_type=="Alley Oop Dunk"]<-"Dunk"
shotlog$shot_type[shotlog$shot_type=="Cutting Dunk Shot"]<-"Dunk"
shotlog$shot_type[shotlog$shot_type=="Running Dunk"]<-"Dunk"
shotlog$shot_type[shotlog$shot_type=="Putback Dunk"]<-"Dunk"
shotlog$shot_type[shotlog$shot_type=="Tip Dunk Shot"]<-"Dunk"
shotlog$shot_type[shotlog$shot_type=="Reverse Dunk"]<-"Dunk"
shotlog$shot_type[shotlog$shot_type=="Running Reverse Dunk Shot"]<-"Dunk"
shotlog$shot_type[shotlog$shot_type=="Driving Reverse Dunk Shot"]<-"Dunk"
shotlog$shot_type[shotlog$shot_type=="Running Alley Oop Dunk Shot"]<-"Dunk"

shotlog$shot_type[shotlog$shot_type=="Driving Hook Shot"]<-"Hook Shot"
shotlog$shot_type[shotlog$shot_type=="Running Hook Shot"]<-"Hook Shot"
shotlog$shot_type[shotlog$shot_type=="Turnaround Hook Shot"]<-"Hook Shot"
shotlog$shot_type[shotlog$shot_type=="Turnaround Bank Hook Shot"]<-"Hook Shot"
shotlog$shot_type[shotlog$shot_type=="Hook Bank Shot"]<-"Hook Shot"

shotlog$shot_type[shotlog$shot_type=="Driving Bank Shot"]<-"Drving Pullup"
shotlog$shot_type[shotlog$shot_type=="Jump Bank Shot"]<-"Bank Shot"
shotlog$shot_type[shotlog$shot_type=="Driving Floating Bank Jump Shot"]<-"Floater"
shotlog$shot_type[shotlog$shot_type=="Turnaround Bank Shot"]<-"Drving Pullup"
shotlog$shot_type[shotlog$shot_type=="Fadeaway Bank Shot"]<-"Drving Pullup"
shotlog$shot_type[shotlog$shot_type=="Pullup Bank Shot"]<-"Catch&Shoot"
shotlog$shot_type[shotlog$shot_type=="Driving Bank Hook Shot"]<-"Drving Pullup"
shotlog$shot_type[shotlog$shot_type=="Turnaround Fadeaway Bank Jump Shot"]<-"Drving Pullup"
shotlog$shot_type[shotlog$shot_type=="Step Back Bank Jump Shot"]<-"Drving Pullup"


unique(shotlog$shot_type)
#work in progress plots
ggplot(shotlog)+
  aes(x=position,fill=team)+
  geom_bar(position = "dodge")+
  labs(title = "How shots are distributed by player position for each team? (tactics)")

threes <- select(filter(shotlog,points==3),colnames(shotlog))
threemade<-select(filter(shotlog,points==3 & outcome=="SCORED"),colnames(shotlog))
ggplot(threemade)+
  aes(x=team)+
  geom_bar()+
  labs(title = "Which team made more 3-pointers? (3-pointer performance)")

ggplot(shotlog)+
  aes(y=shot_type,fill=team)+
  geom_bar(position = "fill")+
  labs(title = "How were shot types distributed for each team? (versatility of offense) ")

clutch<-select(filter(shotlog,quarter==4 & outcome== "SCORED"),colnames(shotlog))
ggplot(clutch)+
  aes(x=team)+
  geom_bar()+
  labs(title = "Which team made more clutch shots ( made attempts in 4th quarter)? (clutch time offensive performance)")
  
madeshots%>%
  group_by(player)%>%
  summarize(total_points=sum(points))%>%
  filter(total_points>1000)%>%
  ggplot(aes(x=player,y=total_points))+
  geom_col()+
  labs(title="Who were their key players? (superstar ability)")

#1st poster plot
ggplot(shotlog)+
  aes(x=position,fill=team)+
  geom_bar(position = "dodge")+
  scale_fill_manual(values = c("maroon","gold"))+
  coord_polar()+
  theme_tufte()

#2nd poster plot

three_accuracy <- threes%>%
  group_by(player)%>%
  summarise(
    three_taken=n(),
    count=1,
    three_outcome=outcome,
    team=team,
    three_accuracy=length(three_outcome[three_outcome=="SCORED"])/three_taken
    )%>%
  arrange(-three_taken)%>%
  subset(three_taken>100) 

three_accuracy$three_accuracy <- round(three_accuracy$three_accuracy,2)
three_accuracy$three_accuracy <- label_percent()(three_accuracy$three_accuracy)
ggplot(three_accuracy,aes(fill=three_outcome,x=reorder(player,three_taken),y=count))+
  geom_bar(position = position_stack(),stat="identity")+
  geom_text(aes(label=three_accuracy),hjust=-10)+
  scale_fill_manual(values = c("maroon","lime green"))+
  theme_tufte()+
  coord_flip()


#3rd poster plot


# plot using ggplot and NBA court background

sc <-  select(filter(shotlog,player=="Stephen Curry"),colnames(shotlog))
kd <- select(filter(shotlog,player=="Kevin Durant"),colnames(shotlog))
lbj <- select(filter(shotlog,player=="LeBron James"),colnames(shotlog))
ki <- select(filter(shotlog,player=="Kyrie Irving"),colnames(shotlog))

ggplot(sc,aes(x=x,y=y))+
  geom_pointrange(aes(colour=outcome))

courtpng<-readPNG("fullcourt.png")

court <- rasterGrob(courtpng,width=unit(1,"npc"), height=unit(1,"npc"))
court
# plot using NBA court background and colour by shot zone
ggplot(sc,aes(x=x,y=y))+
  annotation_custom(court,0,940,0,500)+
  geom_point(aes(colour=outcome))+
  scale_color_manual(values = c("#008000", "#FF6347","#1E90FF")) +
  
  guides(alpha = FALSE, size = FALSE) +
  geom_rug(alpha = 0.2) +
  coord_fixed() +
  ggtitle(paste("Stephen Curry Shot Chart", sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))

ggplot(sc,aes(x=x,y=y))+
  annotation_custom(court,0,940,0,500)+
  stat_binhex(bins = 25,colour="gray",alpha= 0.7)+
  scale_fill_gradientn(colours=c("yellow","orange","red","maroon")) +
  guides(alpha = FALSE, size = FALSE) +
  geom_rug(alpha = 0.2) +
  coord_fixed() +
  ggtitle(paste("Stephen Curry Shot Chart", sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))

ggplot(lbj,aes(x=x,y=y))+
  annotation_custom(court,0,940,0,500)+
  stat_binhex(bins = 25,colour="gray",alpha= 0.7)+
  scale_fill_gradientn(colours=c("yellow","orange","red","maroon")) +
  guides(alpha = FALSE, size = FALSE) +
  geom_rug(alpha = 0.2) +
  coord_fixed() +
  ggtitle(paste("LeBron James Shot Chart", sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))


ggplot(kd,aes(x=x,y=y))+
  annotation_custom(court,0,940,0,500)+
  stat_binhex(bins = 25,colour="gray",alpha= 0.7)+
  scale_fill_gradientn(colours=c("yellow","orange","red","maroon")) +
  guides(alpha = FALSE, size = FALSE) +
  geom_rug(alpha = 0.2) +
  coord_fixed() +
  ggtitle(paste("Kevin Durant Shot Chart", sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))

ggplot(ki,aes(x=x,y=y))+
  annotation_custom(court,0,940,0,500)+
  stat_binhex(bins = 25,colour="gray",alpha= 0.7)+
  scale_fill_gradientn(colours=c("yellow","orange","red","maroon")) +
  guides(alpha = FALSE, size = FALSE) +
  geom_rug(alpha = 0.2) +
  coord_fixed() +
  ggtitle(paste("Kyrie Irving Shot Chart", sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))
#plot 4
clutch<-select(filter(shotlog,quarter==4),colnames(shotlog))
accuracy <- clutch%>%
  group_by(player)%>%
  summarise(
    shot_taken=n(),
    shot_accuracy=length(outcome[outcome=="SCORED"])/shot_taken)%>%
  arrange(-shot_taken)%>%
  slice(-c(31:36))

treemap(accuracy,index = "player",vSize ="shot_taken",vColor = "accuracy",
        palette = brewer.pal(n=8, "Reds"))

ggplot(accuracy,aes(area=shot_taken,
                    fill=shot_accuracy,
                    label=player,
                    subgroup=shot_taken))+
  geom_treemap()+
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE,alpha=3)+
  geom_treemap_subgroup_border(colour="white") +
  geom_treemap_subgroup_text(place="centre",grow = T, alpha = .35, colour =
                               "White", fontface = "italic", min.size = 0)+
  theme_tufte()

#plot5
madeshots<-select(filter(shotlog,outcome=="SCORED"),colnames(shotlog))

types <- madeshots%>%
  group_by(shot_type,team)%>%
  summarise(shot_taken=n())%>%
  arrange(-shot_taken)
ggplot(types,aes(x=shot_taken,y=reorder(shot_type,-shot_taken),fill=team))+
  geom_col(position = "dodge")+
  scale_fill_manual(values = c("maroon","gold"))+
  theme_tufte()
