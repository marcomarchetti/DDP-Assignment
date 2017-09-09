#-----------------------------------
# Project: International Migrations  
# author: Marco Marchetti
# date: September 08, 2017
#----------------------------------

library(shiny)

function(input, output, session) {

library(readxl)
library(plotly)
  
# suppress warnings  
options(warn = -1)
  
#-------------------------
# GETTING & CLEANING DATA 
#-------------------------
  
CMRUrl <-"https://data.unicef.org/wp-content/uploads/2016/09/Child-migrants-and-refugees.xlsx"
ODUrl <- "http://www.un.org/en/development/desa/population/migration/data/estimates2/data/UN_MigrantStockByOriginAndDestination_2015.xlsx"
CRMFile <- "Child-migrants-and-refugees.xlsx"
ODFile  <- "UN_MigrantStockByOriginAndDestination_2015.xlsx"
if (!file.exists(CRMFile)) {
    download.file(CMRUrl, destfile=CRMFile)
  }
if (!file.exists(ODFile)) {
    download.file(ODUrl, destfile=ODFile, mode="wb")
  }

# Read Unicef Data  
CMR2015 <- read_xlsx(CRMFile, col_names = FALSE,
                     sheet="Countries", skip = 11, n_max =197)
colnames(CMR2015) <- c("ISOCode","Countries",
                       "MgrDestTot","MgrDestPerc","MgrDestPercU18", "MgrOrigTot","MgrOrigPerc",
                       "RfgDestTot","RfgDestPercU18","RfgOrigTot","RfgOrigPercU18",
                       "AsylDestTot","AsylOrigTot","IntDisplTot","RatifNum")

# Read UN Data  
OD2015 <- read_xlsx(ODFile, sheet = "Table 16", skip = 15)
names(OD2015)[1]<-"Order"
names(OD2015)[2]<-"Countries"
names(OD2015)[3]<-"Notes"
names(OD2015)[4]<-"Code"
names(OD2015)[5]<-"Type"
OD2015 <- OD2015[ ! is.na(OD2015$Type), ] # remove summary data (Type is NA )
OD2015$Notes <- NULL
OD2015$Order <- NULL
OD2015 <- OD2015[order(OD2015$Countries),]
  
# Country list for plots
countryName <- OD2015$Countries
output$countrySelector <- renderUI({
  selectInput('country', 'Country Name', choices = countryName)
})

#------------
# EMIGRATION 
#------------  

# Total emigration by country  
output$emigSum <- reactive({  
  validate(need(input$country != "", "Please wait"))
  emig <- OD2015[,input$country]
  emig <- as.data.frame(emig)
  names(emig)[1] <- "country"
  emigSum <- sum(emig$country,na.rm=TRUE)
  emigSum <- formatC(emigSum, decimal.mark=",", big.mark=".", digits = 0, format = "f")
  HTML(paste(emigSum,' Total'))
})

# emigration: Refugee by country
output$emigRefStat <- reactive({
    validate(need(input$country != "", "Please wait"))
    if (input$country %in% CMR2015$Countries) {
    emigRefSum <- CMR2015[CMR2015$Countries == input$country,]
    emigRefSum$RfgOrigTot <- as.numeric(emigRefSum$RfgOrigTot)*1000
    if (emigRefSum$RfgOrigPercU18 == '-') {
        emigRefSum$RfgOrigTot <- formatC(emigRefSum$RfgOrigTot, decimal.mark=",", big.mark=".", digits = 0, format = "f")
        HTML(paste(emigRefSum$RfgOrigTot, ' Total', '<br/>','(No Data)', ' Childs'))
    } else {
        emigRefSum$RfgOrigPercU18 <- emigRefSum$RfgOrigTot * as.numeric(emigRefSum$RfgOrigPercU18)/100
        emigRefSum$RfgOrigTot <- formatC(emigRefSum$RfgOrigTot, decimal.mark=",", big.mark=".", digits = 0, format = "f")
        emigRefSum$RfgOrigPercU18 <- formatC(emigRefSum$RfgOrigPercU18, decimal.mark=",", big.mark=".", digits = 0, format = "f")
        HTML(paste(emigRefSum$RfgOrigTot, ' Total', '<br/>',emigRefSum$RfgOrigPercU18, ' Childs'))
    }
  }
  
})

# emigration: Asylum by country  
output$emigAsyStat <- reactive({
  validate(need(input$country != "", "Please wait"))
  if (input$country %in% CMR2015$Countries) {
      emigAsySum <- CMR2015[CMR2015$Countries == input$country,]
     if (emigAsySum$AsylOrigTot == '-') {
          HTML(paste('(No Data)', ' Seekers'))
     } else {
          emigAsySum$AsylOrigTot <- as.numeric(emigAsySum$AsylOrigTot)*1000
          emigAsySum$AsylOrigTot <- formatC(emigAsySum$AsylOrigTot, decimal.mark=",", big.mark=".", digits = 0, format = "f")
          HTML(paste(emigAsySum$AsylOrigTot, ' Seekers'))
     }
  }
})

#-------------
# IMMIGRATION 
#-------------

# Total immigration by country  
output$immigSum <- reactive({ 
  validate(need(input$country != "", "Please wait"))
  #Total
  immig <- OD2015[OD2015$Countries == input$country,]
  immig[,1:6] <- NULL
  namesImmig <- names(immig)
  immig <- t(immig)
  immig <- as.data.frame(immig)
  names(immig)[1]<-"country"
  immigSum <- sum(immig$country,na.rm=TRUE)
  # Childs
  if (input$country %in% CMR2015$Countries) {
      immigChildSum <- CMR2015[CMR2015$Countries == input$country,]
      if (immigChildSum$MgrDestPercU18 == '-') {
        immigSum <- formatC(immigSum, decimal.mark=",", big.mark=".", digits = 0, format = "f")
        HTML(paste(immigSum,' Total','<br/>','(No Data)',' Childs'))
      } else {
        immigChildSum$MgrDestPercU18 <- immigChildSum$MgrDestTot*1000 * as.numeric(immigChildSum$MgrDestPercU18)/100
        immigSum <- formatC(immigSum, decimal.mark=",", big.mark=".", digits = 0, format = "f")
        immigChildSum$MgrDestPercU18  <- formatC(immigChildSum$MgrDestPercU18 , decimal.mark=",", big.mark=".", digits = 0, format = "f")
        HTML(paste(immigSum,' Total','<br/>',immigChildSum$MgrDestPercU18,' Childs'))
      }
  }
})

# immigration: Refugee by country  
output$immigRefStat <- reactive({
  validate(need(input$country != "", "Please wait"))
  if (input$country %in% CMR2015$Countries) {
      immigRefSum <- CMR2015[CMR2015$Countries == input$country,]
      immigRefSum$RfgDestTot <- as.numeric(immigRefSum$RfgDestTot)*1000
      if (immigRefSum$RfgDestPercU18 == '-') {
          immigRefSum$RfgDestTot <- formatC(immigRefSum$RfgDestTot, decimal.mark=",", big.mark=".", digits = 0, format = "f")
          HTML(paste(immigRefSum$RfgDestTot, ' Total', '<br/>','(No Data)', ' Childs'))
      } else {
          immigRefSum$RfgDestPercU18 <- immigRefSum$RfgDestTot * as.numeric(immigRefSum$RfgDestPercU18)/100
          immigRefSum$RfgDestTot <- formatC(immigRefSum$RfgDestTot, decimal.mark=",", big.mark=".", digits = 0, format = "f")
          immigRefSum$RfgDestPercU18 <- formatC(immigRefSum$RfgDestPercU18, decimal.mark=",", big.mark=".", digits = 0, format = "f")
          HTML(paste(immigRefSum$RfgDestTot, ' Total', '<br/>',immigRefSum$RfgDestPercU18, ' Childs'))
      }
  }
})

# immigration: Asylum by country  
output$immigAsyStat <- reactive({
  validate(need(input$country != "", "Please wait"))
  if (input$country %in% CMR2015$Countries) {
      immigAsySum <- CMR2015[CMR2015$Countries == input$country,]
      if (immigAsySum$AsylDestTot == '-') {
          HTML(paste('(No Data)', ' Seekers'))
       } else {
          immigAsySum$AsylDestTot <- as.numeric(immigAsySum$AsylDestTot)*1000
          immigAsySum$AsylDestTot <- formatC(immigAsySum$AsylDestTot, decimal.mark=",", big.mark=".", digits = 0, format = "f")
          HTML(paste(immigAsySum$AsylDestTot, ' Seekers'))
       }
  }
})

#-----------------------
# INTERNAL DISPLACEMENT 
#-----------------------

output$intgDispStat <- reactive({
  validate(need(input$country != "", "Please wait"))
  if (input$country %in% CMR2015$Countries) {
      intDispSum <- CMR2015[CMR2015$Countries == input$country,]
      if (intDispSum$IntDisplTot == '-') {
          HTML(paste('(No Data)', ' Displaced'))
      } else {
          intDispSum$IntDisplTot <- as.numeric(intDispSum$IntDisplTot)*1000
          intDispSum$IntDisplTot <- formatC(intDispSum$IntDisplTot, decimal.mark=",", big.mark=".", digits = 0, format = "f")
          HTML(paste(intDispSum$IntDisplTot, ' Displaced'))
      }
  }
})

#------------
#PLOTS
#------------

# Plot emigration by Country
output$emigration <- renderPlotly({
  validate(need(input$country != "", "Please wait"))
  # data selection and sum
  emig <- OD2015[,input$country]
  emig <- as.data.frame(emig)
  names(emig)[1] <- "country"
  emigSum <- sum(emig$country,na.rm=TRUE)
  
  # Set up mapping options
  graphTitle  <- paste("Golbal Emigration from ", input$country, sep="")  
  borders <- list(color = toRGB("black"))
  map_options <- list(
      scope = 'world',
      projection = list(type = 'mercator'),
      showlakes = TRUE,
      lakecolor = toRGB('white'),
      margin = list(pad = 1, l = 1, r = 1, b =1, t=1))

  # Plot
  plot_ly(z = ~emig$country, text = ~countryName, locations = ~countryName, 
          type = 'choropleth', locationmode = 'country names', 
          color = ~emig$country, colors = 'Blues', marker = list(line = borders),
          name = " ", showlegend = FALSE) %>%
  colorbar(title = 'People num') %>%
  layout(title = graphTitle, geo = map_options) %>%
  add_trace(z = emigSum, text = input$country, locations = input$country, 
            type = 'choropleth', locationmode = 'country names', colors = 'Red', showscale = FALSE) 
})

# Plot immigration by Country
output$immigration <- renderPlotly({    
  validate(need(input$country != "", "Please wait"))
  # data selection and sum
  immig <- OD2015[OD2015$Countries == input$country,]
  immig[,1:6] <- NULL
  namesImmig <- names(immig)
  immig <- t(immig)
  immig <- as.data.frame(immig)
  names(immig)[1]<-"country"
  immigSum <- sum(immig$country,na.rm=TRUE)

  # Set up mapping options
  graphTitle  <- paste("Golbal Immigration to ", input$country, sep="")  
  borders <- list(color = toRGB("black"))
  map_options <- list(
    scope = 'world',
    projection = list(type = 'mercator'),
    showlakes = TRUE,
    lakecolor = toRGB('white'),
    margin = list(pad = 1, l = 1, r = 1, b =1, t=1))
  
  # Plot  
  plot_ly(z = ~immig$country, text = ~countryName, locations = ~countryName, 
          type = 'choropleth', locationmode = 'country names', 
          color = ~immig$country, colors = 'Blues', marker = list(line = borders),
          name = " ", showlegend = FALSE) %>%
  colorbar(title = 'People num', tickprefix = '') %>%
  layout(title = graphTitle, geo = map_options ) %>%
  add_trace(z = immigSum, text = input$country, locations = input$country, 
            type = 'choropleth', locationmode = 'country names', colors = 'Red', showscale = FALSE) 
  })

}
