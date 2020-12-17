#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
setwd("C:/Users/carte/OneDrive/Desktop/IST 421")

library(maps)
library(mapproj)
library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
wineUI <- fluidPage(

    # Application title
    titlePanel("ACME Wine Company Dashboard"),
    sidebarLayout(
        sidebarPanel(
            tags$style(".well {background-color:#A0A0A0;}"),
            plotOutput("yearlySales"),
            plotOutput("units.distribution")
        ),
        mainPanel(
            selectInput(
                "region",
                "Select region",
                choices = c("All", "North", "East", "Central",
                            "South", "West")
            ),
            selectInput(
                "year",
                "Select year",
                choices = c("All", "2010", "2011",
                            "2012", "2013", "2014")
            ),
            plotOutput("wine.region"),
            plotOutput("region.map")
        )
    )
)

# Define server logic required to draw a histogram
wineServer <- function(input, output) {
    sales<-read_csv("sales.csv")
    regions.df <- read_csv("wine.regions.csv")
    
    #1st Plot
    output$yearlySales<- renderPlot({
        
        print(paste("Year:",input$year))
        print(paste("Region:",input$region))
        
        sales %>% group_by(rep.region,year) %>%
            summarise(receipt=sum(recipt)) %>%
            ggplot(aes(x=year,y=receipt,color=rep.region))+
            geom_line(size=2)+ylim(2,100000)+
            ggtitle("Sales Revenue by Region")+
            theme_minimal()+
            theme(legend.position = "bottom")
    })
    
    #2nd Plot
    output$wine.region <- renderPlot({
        sales.2<- sales
        region.name<-"All"
        my.year<-"All"
        
        if (input$region != "All") {
            sales.2 <- sales.2 %>% filter(rep.region == input$region)
            region.name <- input$region
        }
        if (input$year != "All") {
            sales.2 <- sales.2 %>% filter(year == input$year)
            my.year <- input$year
        }
        
        sales.2 %>%
            group_by(wine,type) %>%
            summarise(recipt = sum(recipt)) %>%
            ggplot(aes(x=wine,y=recipt,fill=type)) +
            geom_col(position = "dodge") +
            theme_minimal()+
            scale_fill_manual(values = c("#B62D23","#F8D16D"))+
            ggtitle(paste("Wine Sales for region",region.name,", year:",my.year))
    })
    
    #3rd Plot
    output$region.map <- renderPlot({
        states_map <- map_data("state")
        df <- left_join(states_map,regions.df,by="region")
        
        ggplot(df,aes(long,lat,group=group))+
            geom_polygon(aes(fill=rep.region),color="white")+
            labs(x="",y="",title = "Wine Sales Regions in the US",
                 subtitle = "Shows which states are in which regions",
                 caption = "IST 421 Pseudo Data")+
            theme(legend.position = "none",
                  axis.text = element_blank(),
                  axis.ticks= element_blank(),
                  panel.background = element_blank())+
            coord_map("albers",lat0=45.5,lat1=29.5)
    })
    
    #4th Plot
    output$units.distribution <- renderPlot({
        ggplot(sales)+
            aes(x=rep.region,y=units.sold,
                fill=rep.region,
                color=rep.region)+
            geom_violin()+
            theme_minimal()+
            theme(legend.position = "none")
    })
    
}

# Run the application 
shinyApp(ui = wineUI, server = wineServer)
