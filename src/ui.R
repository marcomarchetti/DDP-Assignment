#-----------------------------------
# Project: International Migrations  
# author: Marco Marchetti
# date: September 08, 2017
#----------------------------------

library(plotly)
library(shiny)

shinyUI(
  navbarPage("International Migrations (2015)",
     tabPanel("Plot", 
              tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
              sidebarPanel(uiOutput("countrySelector"),
                          hr(),
                          h3("Emigration"),
                          h4("Num. of Emigrants"),htmlOutput("emigSum"),
                          h4("Num. of Refugee"), htmlOutput("emigRefStat"),
                          h4("Num. of Asylum Seekers"),htmlOutput("emigAsyStat"),
                          hr(),
                          h3("Immigration"),
                          h4("Num. of Immigrants"), htmlOutput("immigSum"),
                          h4("Num. of Refugee"),htmlOutput("immigRefStat"),
                          h4("Num. of Asylum Seekers"),htmlOutput("immigAsyStat"),
                          hr(),
                          h3("Internal Displacement"),htmlOutput("intgDispStat")
             ),
             mainPanel(
                       tabsetPanel(
                          tabPanel(p(icon("globe"), "Immigration"),
                                   HTML("<center> <b> <br>"),
                                   tags$div(HTML(paste(tags$span(style="color:darkblue", "ORIGIN  COUNTRIES"), 
                                            " ==> ", tags$span(style="color:red", "DESTINATION COUNTRY"), sep = ""))),
                                   HTML("</b></center><br>"),
                                   plotlyOutput("immigration", width = 700, height = 700)
                          ),
                          tabPanel(p(icon("globe"), "Emigration"),
                                   HTML("<center> <b> <br>"),
                                   tags$div(HTML(paste(tags$span(style="color:red", "ORIGIN  COUNTRY"), 
                                            " ==> ", tags$span(style="color:darkblue", "DESTINATION COUNTRIES"), sep = ""))),
                                   HTML("</b></center><br>"),
                                   plotlyOutput("emigration", width = 700, height = 700)
                          )
                        )
              )
      ),
      tabPanel("About", mainPanel(includeMarkdown("about.md")))
  )
)
            