library(tidyverse)
tweets <- read_csv("ClimateTweets_June2016.csv")

dim(tweets)
head(tweets)

table(tweets$media)


ifelse(is.na(tweets$media), "text", tweets$media)
replace_na(tweets$media, "text")



tweets <- tweets %>% mutate(media=replace_na(media,"text"))
table(tweets$media)
gsub(pattern = "photo(?:\\|photo)*","photo",tweets$media)

tweets <- tweets %>% 
  mutate(media = gsub(pattern = "photo(?:\\|photo)*","photo",media))
table(tweets$media)

ggplot(tweets)+geom_bar(aes(media))

tweets$created_at[1:3]

#Match this

tweets$posted.date <- as.POSIXct(
  strptime(tweets$created_at, "%a %b %d %H:%M:%S +0000 %Y"))

library(lubridate)
ymd("2010-10-1")
ymd("2010-10:1")
ymd("2010 10 1")

mdy("01 03 73")

mdy_hm("03/01/1989 12:32 AM")
mdy_hms("03/01/1989 12:32:32 AM")

parse_date_time(tweets$created_at,order="%a %b %d %H:%M:%S +0000 %Y", exact = T)

max(tweets$posted.date)
min(tweets$posted.date)

difftime(max(tweets$posted.date),min(tweets$posted.date),units = "weeks")
difftime(max(tweets$posted.date),min(tweets$posted.date),units = "hours")
difftime(max(tweets$posted.date),min(tweets$posted.date),units = "months")

ggplot(tweets)+geom_bar(aes(wday(tweets$posted.date,abbr = T, label = T)))
ggplot(tweets)+geom_bar(aes(hour(tweets$posted.date)))

install.packages("circular")
library(circular)

tmp<-circular(hour(tweets$posted.date)%%24,
              units = "hour",
              template = "clock24")
rose.diag(tmp,bins = 24, prop = 3.5)

install.packages("wordcloud")
install.packages("plotrix")
library(plotrix)
library(wordcloud)

tags <- str_extract_all(tweets$text, "#\\S+", FALSE)
tags <- tags[length(tags)>0]
tags <- unlist(tags,use.names = F)
tags <- tolower(gsub("#|[[:punct:]]","",tags))

tags_df <- tibble(tags = tags)
tags_df <- tags_df %>% group_by(tags) %>% summarise(count=n())
tags_df <- tags_df %>% filter(count>1) %>% arrange(-count)
dim(tags_df)

sm_tags <- tags_df[1:200,]

myPalFun <- colorRampPalette(c("gold","orange","red"))

par(mar=c(0,0,0,0))
wordcloud(sm_tags$tags, sm_tags$count,scale = c(6,.75),
          min.freq = 1, max.words = Inf, random.color = F, ordered.colors = T, rot.per = 0,
          colors = myPalFun(nrow(sm_tags)))

sm_tags <- sm_tags %>% mutate(scaled = rescale(count,c(0,1)))
sm_tags <- sm_tags %>% mutate(scaled1 = rescale(count,c(0,1))^0.5)
sm_tags <- sm_tags %>% mutate(scaled2 = rescale(count,c(0,1))^2)
sm_tags <- sm_tags %>% mutate(scaled3 = rescale(log10(count),c(0,1)))
sm_tags <- sm_tags %>% mutate(index = row_number())

ggplot(sm_tags)+geom_line(aes(index,scaled))+
  geom_line(aes(index,scaled1),color="blue")+
  geom_line(aes(index,scaled2),color="green")+
  geom_line(aes(index,scaled3),color="red")
  
  
wordcloud(sm_tags$tags, sm_tags$scaled3*50, scale=c(2,.25),
          min.freq = 1, max.words = Inf, random.order = F,
          random.color = F, ordered.colors = T, rot.per = .35,
          colors = myPalFun(nrow(sm_tags)))


