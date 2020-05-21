library(shiny)
library(leaflet)
library(dplyr)
library(plyr)
library(ggplot2)
library(maptools)


shinyServer(function(session,input, output) {


output$map <- renderLeaflet({
    bins <- as.vector(trunc(quantile(Italia@data[[gsub(" ","_",input$variabile)]],probs=c(0,0.2,0.35,0.5,0.65,0.75,0.85,0.95,0.98,1))))
    pal <- colorBin("YlOrRd", domain = Italia@data[[gsub(" ","_",input$variabile)]], bins = bins)
    dat<-get(gsub(" ","_",input$scope))
    leaflet()%>%
    addTiles()%>%
    addPolygons(data=dat,fillColor=~pal(dat@data[[gsub(" ","_",input$variabile)]]),label=paste(input$variabile,": ",dat@data[[gsub(" ","_",input$variabile)]]),weight=0.5,opacity=1,color="grey",fillOpacity = 0.5,highlightOptions = highlightOptions(color = "white",weight = 2,bringToFront = TRUE))%>%
    addLegend(pal=pal, values=dat@data[[gsub(" ","_",input$variabile)]],title=paste(input$scope, input$variabile))
})
})

