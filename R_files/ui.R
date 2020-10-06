library(shiny)
library(shinyWidgets)
library(dygraphs)
library(shinyalert)
source("map.R")

navbarPage("Italia COVID-19" ,tabsetPanel(
        tabPanel("Italia",
                 fluidRow(
                         column(
                                 width=2,wellPanel(
                                         fluidRow(column(width=11, radioButtons("scope"," ",c("Italia","Nord","Centro","Sud e Isole")),align="left")),
                                         fluidRow(column(width=11,dateInput("day","Data", min=region_dataset$data[1], max=region_dataset$data[nrow(region_dataset)], format="dd/mm/yyyy",value=region_dataset$data[nrow(region_dataset)]))),
                                         fluidRow(column(width=11, selectInput("variabile","Parametro",gsub("_"," ",names(select(region_dataset,7:15,18:20,23:32))),selected=NULL)
                                         )),
                                 )),
                         conditionalPanel(condition= ("(input.variabile != 'casi testati') && (input.variabile != 'variazione casi testati')&&(input.variabile != 'p casi testati')&& (input.variabile != 'n casi testati normalizzato')&& (input.variabile != 'p nuovi positivi')|| (input.day > '2020-04-18')"),
                                          column(
                                                  width=5,
                                                  leafletOutput(outputId = "map",width="100%",height = 750)
                                                  ,align="left"),
                                          column(
                                                  width=5,align="left", panel(
                                                          fluidRow(column(dygraphOutput("plot1",height = 280),width=11),align="left"),
                                                          hr(),
                                                          plotlyOutput("plot2",height=400)
                                                  ))
                         ),
                         conditionalPanel(condition=(("input.day < '2020-04-19') && (input.variabile =='casi testati'|| input.variabile =='variazione casi testati'|| input.variabile =='p casi testati'|| input.variabile =='n casi testati normalizzato'|| input.variabile =='p nuovi positivi'")),
                                          column(width=8, align ="center",
                                                 textOutput("text", h2),
                                                 textOutput("text1",h5))
                         ))),
        tabPanel("Monitoring",
                 fluidRow(
                         column(
                                 width=2,wellPanel(
                                         fluidRow(column(width=11,selectInput("var","Carta di controllo",c("nuovi positivi","n positivi giornalieri normalizzato","n casi testati normalizzato")))),
                                         fluidRow(column(width=11, selectInput("reg","Scegli regione",unique(region_dataset[,1]),selectize=FALSE))),
                                         fluidRow(column(width=11,dateInput("date1","Inizio Fase 1",format="dd/mm/yyyy",value="2020/05/04"))),
                                                                            #,min="2020/05/04",max=region_dataset$data[nrow(region_dataset)],format="dd/mm/yyyy",value="2020/05/04"))),
                                        fluidRow(column(width=11,dateInput("date2","Inizio Fase 2",value="2020/05/31",format="dd/mm/yyyy"))),
                                                        #dateInput("date2","Inizio Fase 2",min="2020/05/04",max=region_dataset$data[nrow(region_dataset)],format="dd/mm/yyyy",value="2020/05/31")))),
                                        conditionalPanel(condition=("input.var=='n casi testati normalizzato'"),
                                         fluidRow(column(width=11,uiOutput("target"))))),
                                        htmlOutput("text5")
                                 ),
                         column(align="center",
                                 width=5,panel(
                                 conditionalPanel(condition=("input.var=='nuovi positivi'"),
                                        dygraphOutput("chart1"),
                                        textOutput("text6",h5)
                                        #dygraphOutput("chart2")
                                        ),
                                 conditionalPanel(condition=("input.var=='n positivi giornalieri normalizzato'"),
                                                  dygraphOutput("chart2"),
                                                  textOutput("text7",h5)
                                 ),
                                 conditionalPanel(condition=("input.var=='n casi testati normalizzato'"),
                                         dygraphOutput("chart5"),
                                         textOutput("text9",h5)),
                                 conditionalPanel(condition=("input.var=='n casi testati normalizzato'"),
                                                  dygraphOutput("chart6"),
                                                  )
                         )
                 )))
        ))


