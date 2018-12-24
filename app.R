#Econometrics Final Project
#A simple UI to enable Business Analysts to perform EDA on the Car Fatalities Data

#Created By: Mohit Sahasrabudhe
#Date Created: 12 December 2018


#Add some libraries
library(shiny)
library(foreign)
library(plm)
library(dplyr)
library(choroplethr)


#Read the data file
car_fatalities<-read.dta("car_fatalities.dta")



#UI Description
ui <- fluidPage(
  titlePanel("Exploratory Data Analysis - Fatalities"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Year:",
                  min = 1982,
                  max = 1988,
                  value = 1982),
      selectInput("columnName","Select the Metric",choices=colnames(car_fatalities)[!colnames(car_fatalities) %in% c("state","year")] )
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)


#UI Description
server <- function(input, output) {
  output$distPlot <- renderPlot({
    
    
    
    year_x=input$bins
    stateWiseDistClient<-data.frame(state=car_fatalities$state,population=car_fatalities[,input$columnName],year=car_fatalities$year)
    colnames(stateWiseDistClient)<-c("region","value","year")
    stateLookUp<-data.frame(cbind(state.abb,state.name))
    stateWiseDistClient<-left_join(stateWiseDistClient,stateLookUp,by=c("region" = "state.abb"))
    stateWiseDistClient<-stateWiseDistClient[stateWiseDistClient$year==year_x,c("value","state.name")]
    colnames(stateWiseDistClient)<-c("value","region")
    stateWiseDistClient$region<-tolower(stateWiseDistClient$region)
    
    
    
    #Plot the chloropleth
    state_choropleth(stateWiseDistClient)
  })
}


#Render Application
shinyApp(ui = ui, server = server)
