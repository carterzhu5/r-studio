#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
wineui <- fluidPage(
  titlePanel("Wine Company Data"),
  sidebarLayout(
    sidebarPanel(tags$style(".well {background-color:#A0A0A0}"),
                 plotOutput("yearlySales"),
                 plotOutput("units.distribution")
                 selectInput("region","Select Region:",
                             choices=c("All","North","West","Central","South","East")),
                 selectInput("year","Select Year:", choices = c("All","2010","2011","2012","2013","2014"))
    ),
    mainPanel(plotOutput("wine.by.region"),
              plotOutput("region.map")))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  sales <- read_csv("sales.csv")
  
  output$yearlySales <- renderPlot({
    
    sales %>% group_by(rep.region,year) %>% 
      summarise(recipt = sum(recipt)) %>%
      ggplot(aes(x=year,y=recipt,color=rep.region)) +
      geom_line(size=2) + ylim(c(0,100000)) +
      ggtitle("Sales Revenue by Region") +
      theme_minimal() +
      theme(legend.position="bottom")
  })
}

    
    print(paste("The year selected is",input$year))
    print(paste("Region: ",input$region))
    
    sales.2<-sales.2
    region.name <- "All"
    my.year <- "All"
    
    if(input$region !="All"){
      sales.2 <- sales.2%>%filter(rep.region==input$region)
      region.name <- input$region
    }
    
    if(input$region !="All"){
      sales.2 <- sales.2%>%filter(year==input$year)
      my.year <- input$year
    }
    
    sales.2 %>%
      group_by(wine,type)%>%
      summarise(recipt=sum(recipt))%>%
      ggplot(aes(x=wine,y=recipt,fill=type))+
      geom_col(position = "dodge")+
      theme_minimal()+
      ggtitle(paste("Wine Sales for Region: ",region.name,", year:",my.year))
    
  
    
    output$region.map <- renderPlot({
      states_map <- map_data("state")
      df <- left_join(states_map,regions.df,by="region")
      ggplot(df,aes(long,lat,group=group))+
        geom_polygon(aes(fill=rep.region),color="white")
        labs(x="",y="",
             title="Wine Sales Regions in th US",
             subtitle="Shows which states are in which regions",
             caption="IST 421 Pseudo Data")+
        theme(legend.position = "none",
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.background = element_blank())+
        coord_map("albers",at0=45.5,lat1=29.5)
    })
    
      
    sales %>% group_by(rep.region,year) %>% 
      summarise(recipt = sum(recipt)) %>%
      ggplot(aes(x=year,y=recipt,color=rep.region)) +
      geom_line(size=2) + ylim(c(0,100000)) +
      ggtitle("Sales Revenue by Region") +
      theme_minimal() +
      theme(legend.position="bottom")
    
    output$units.distribution <- renderPlot({
      ggplot(sales,aes(x-rep.region,y=units.sold,fill=rep.region,color=rep.region))+
        geom_violin()+
        theme_minimal()+
        theme(legend.position = "none")
    })

# Run the application 
shinyApp(ui = wineui, server = server)

