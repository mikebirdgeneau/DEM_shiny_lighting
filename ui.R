
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyIncubator)

library(lubridate)
library(ggplot2)
library(raster)
library(RColorBrewer)
library(rgdal)

shinyUI(fluidPage(
  # Initialize Progress Bar
  #progressInit(),

  # Application title
  titlePanel("Digital Elevation Map Lighting"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("mon",
                  "Month:",
                  min = 1,
                  max = 12,
                  value = month(Sys.Date())),
      sliderInput("dy",
                  "Day of Month:",
                  min = 1,
                  max = 31,
                  value = day(Sys.Date())),
      sliderInput("hr",
                  "Hour (MDT):",
                  min = 0,
                  max = 23,
                  value = hour(Sys.time())),
      helpText("Note: Please use the sliders above to set the desired date / time for upating the lighting for the map, then click the button below to update the map. Note that processing may take 15-60 seconds if the image needs to be created."),
      submitButton("Update Map")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      imageOutput("demPlot")
    )
  )
))
