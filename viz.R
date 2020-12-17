library(tidyverse)

library(readxl)
library(ggplot2)
library(readr)
library(dplyr)
library(RColorBrewer)
library(tidyr)
library(viridis)
library(hrbrthemes)
storm_data<-read_csv("StormEvents.gz",col_types = cols(EPISODE_NARRATIVE="c",EVENT_NARRATIVE="c",WFO='c',EPISODE_ID='c',TOR_OTHER_CZ_STATE='c',SOURCE='c',BEGIN_LOCATION='c',END_LOCATION='c',TOR_OTHER_CZ_FIPS='c',TOR_OTHER_CZ_NAME='c',BEGIN_AZIMUTH='c',END_AZIMUTH='c',DAMAGE_PROPERTY="c",DAMAGE_CROPS='c',MAGNITUDE_TYPE='c',FLOOD_CAUSE='c',TOR_OTHER_WFO='c',CATEGORY='i'))

unique(storm_data$EVENT_TYPE)

storm_type_count<-data.frame(count(storm_data,EVENT_TYPE))
storm_type_count
storm_type_count%>% top_n(10)%>%
  arrange(desc(n))%>%
  ggplot(.,aes(x=reorder(EVENT_TYPE,n),y=n))+
  geom_col()


