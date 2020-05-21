library(shiny)
library(shinyWidgets)

source("map.R")

shinyUI(fluidPage(
  tabPanel("Component 1",
    # Application title
    titlePanel("Italia COVID-19"),

   sidebarLayout(
       sidebarPanel(
          radioButtons("scope","Scegli",c("Italia","Nord","Centro","Sud e Isole")
                       ),
          selectInput("variabile","Scegli",gsub("_"," ",names(region_latest_dataset[7:18])),selected=NULL
                       ),
          dateInput("data","Scegli", min=region_dataset$data[1], max=region_dataset$data[nrow(region_dataset)]
           )
            
        ),
       

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput(outputId = "map",width="100%")
      )
    )
),tabPanel("component2")))
