rm(list=ls())
setwd("D:/IST 421")
library(tidyverse)
?mpg
str(mpg)

ggplot(data=mpg)+geom_point(aes(x=displ,y=hwy))
ggplot()+geom_point(mpg,mapping = aes(x=displ,y=hwy))
ggplot(mpg,aes(x=displ,y=hwy))+geom_point()+geom_text(aes(label=manufacturer))

#ggplot is an object
g<-ggplot(mpg,mapping = aes(displ,hwy))
g+geom_point()
g+geom_col()
g<-g+geom_point()
g

#decorating plots
ggplot(mpg)+geom_point(aes(displ,hwy))+
  theme(axis.title = element_text(size=20),
        axis.line.x = element_line(size = 1,arrow = arrow()),
        axis.line.y=element_line(size=2))

ggplot(mpg)+geom_point(aes(displ,hwy,color=class))
ggplot(mpg)+geom_point(aes(displ,hwy,alpha=class))
ggplot(mpg)+geom_point(aes(displ,hwy,size=class))
ggplot(mpg)+geom_point(aes(displ,hwy,shape=class))
ggplot(mpg)+geom_point(aes(displ,hwy,color=class,shape=class))

#fixed setting
ggplot(mpg)+geom_point(aes(displ,hwy),color="blue")

tmp<-1:dim(mpg)
tmp
ggplot(mpg)+geom_point(aes(displ,hwy,color=tmp))

ggplot(mpg)+
  geom_point(aes(displ,hwy,color=class))

ggplot(mpg,aes(displ,hwy))+geom_smooth()+geom_point()

ggplot(mpg,aes(displ,hwy))+geom_smooth()+geom_point(aes(color=class))

ggsave("lab6_plot1.pdf")

ggplot(mpg,aes(displ,hwy))+
  geom_smooth(aes(linetype-class))+
  geom_point(aes(color=class))
                                                                       

#statistical transformations

ggplot(data = mpg)+geom_bar(aes(x=class))

ggplot(data=mpg)+
  stat_count(aes(x=class))
d2a<-aggregate(list(count=mpg$class),by=list(class=mpg$class),FUN=length)
d2a

d2<-mpg %>% group_by(class)%>% summarise(count=n())
ggplot(d2)+geom_col(aes(class,count))

d2a<-d2a[order(-d2a$count),]

ggplot(d2a)+
  geom_col(aes(class,count))

d2a$class<-factor(d2a$class,d2a$class)
ggplot(d2a)+
  geom_col(aes(class,count))

ggplot(mpg)+stat_count(aes(x=class,fill=trans))

ggplot(mpg)+stat_count(aes(x=class,fill=trans),position = "dodge")

ggplot(mpg)+stat_count(aes(x=class,fill=trans),
                       position = position_dodge(preserve = "single"))

ggsave("lab6_plot2.pdf")

f<-colorRampPalette(c("blue","yellow","red"))

types<-unique(mpg$trans)
pal<-f(length(types))

ggplot(mpg)+stat_count(aes(x=class,fill=trans),
                       position = position_dodge(preserve = "single"))+
  scale_fill_brewer(palette = "Set3")

install.packages("viridis")
library(viridis)

ggplot(mpg)+stat_count(aes(x=class,fill=trans),
                       position = position_dodge(preserve = "single"))+
  scale_fill_viridis(discrete=T,option="magma")

#turning off legends
ggplot(mpg)+stat_count(aes(x=class,fill=trans),
                       position = position_dodge(preserve = "single"))+
  scale_fill_viridis(discrete=T,option="magma")+theme(legend.position = "none")


#using faceting
ggplot(mpg)+geom_point(aes(displ,hwy,color=trans),size=1)+
  scale_color_viridis(option="magma",discrete = T)+
  ggtitle("Highway MPG x Displacement")+
  facet_grid(cyl~trans)+theme_minimal()+
  theme(legend.position = "none")

ggsave("lab6_plot3.pdf")

install.packages("nycflights13")
library(nycflights13)
write_csv(flights,"nycflights.csv")
f<-read_csv("nycflights.csv")  
is_tibble(f)

f2<-read.csv("nycflights.csv",header = T,stringsAsFactors = F)
is_tibble(f2)

by_dest<-group_by(f,dest)
delay<-summarize(by_dest,count=n(),dist=mean(distance,na.rm = T),
                 delay=mean(arr_delay,na.rm = T))
delay<-filter(delay,count>20)

#use the pipe command
delays<-f %>% group_by(dest)%>%
  summarize(count=n(),dist=mean(distance,na.rm = T),
            delay=mean(arr_delay,na.rm = T))%>%
filter(count>20)  
delays

ggplot(delays,aes(count,delay))+geom_point()
ggplot(delays,aes(count,delay))+geom_point()+
  geom_label(aes(label=dest))

install.packages("ggrepel")
library(ggrepel)

ggplot(delays,aes(count,delay))+geom_point()+
  geom_label_repel(aes(label=dest))

ggplot(delays,aes(count,delay,color=dist))+geom_point()+
  geom_label_repel(data=delays %>% filter(count>7500),aes(label=dest))+
  scale_color_viridis()

ggsave("lab6_plot4.pdf")

f %>% group_by(year,month,day) %>% summarise(mean=mean(dep_delay))

f %>% group_by(year,month,day) %>% summarise(mean=mean(dep_delay,na.rm = T))

f %>% group_by(year,month,day) %>% summarise(mean=sum(dep_delay))

f %>% group_by(year,month,day) %>% 
  filter(!is.na(dep_delay)) %>%
  summarise(mean=mean(dep_delay,na.rm = T))

f %>% group_by(year,month,day) %>% 
  summarise(mean=mean(dep_delay,na.rm = T)) %>%
  mutate(date=as.Date(paste(year,month,day,sep = "-"))) %>%
  ggplot(aes(date,mean))+geom_line()+geom_smooth()+
  ggtitle("Delays by Date")

ggsave("lab6_plot5.pdf")

not_cancelled<-f %>% filter(is.na(dep_delay),is.na(arr_delay))
delays<-not_cancelled %>% group_by(tailnum) %>%
  summarise(delay=mean(arr_delay))

delays<-not_cancelled %>% group_by(tailnum) %>%
  summarise(delay=mean(arr_delay),num.flights=n())

ggplot(delays,aes(delay,num.flights))+geom_point()

f %>% group_by(tailnum) %>% 
  summarise(total.air.time=sum(air_time,na.rm = T),
            num.flights=n()) %>%
  ggplot(aes(num.flights,total.air.time))+
  geom_point(size=2)

f %>% mutate(route=paste(origin,dest,sep="_"))%>%
  group_by(route,tailnum)%>%
  summarise(total.air.time=sum(air_time,na.rm = T),
            num.flights=n()) %>%
  ggplot(aes(num.flights,total.air.time,color=route))+
  geom_point(size=2)+
  theme(legend.position = "none")

ggsave("lab6_plot6.pdf")
