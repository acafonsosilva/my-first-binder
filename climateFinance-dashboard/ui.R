library(shiny)
library(shinyFiles)
library(tidyverse)
library(readxl)
library(R3port)
library(gt)
library(psycModel)
library(lubridate)

path <- this.path::this.dir()

# Define UI for application that draws a histogram
fluidPage(
  titlePanel("Producing Climate Finance tables"),
  
  # Sidebar with two input widgets
  sidebarLayout(
    sidebarPanel(
      
      p("1. Select folder with design elements from illustrator"),
      p("eg. R_DesignElements"),
      p(""),
      shinyDirButton(id = "iconsFolder", label = "Select icons", title = "Select directory with icons"), ### to add Icons
      p(""),
      p("2. Upload climate finance file"),
      p("eg. CAT_ClimateFinance.xlsx - sheet ClimateFinance-Subratings"),
      fileInput(inputId = "dataset",
                label = "Find excel file", accept = c(".xlsx")),       ### to add dataset
      p("3. Choose the Country to export"),  
      selectInput(inputId = "countryVariable", label = "Select Country", choices = NULL),
      p("4. Write date label"), 
      textInput("date", "Date", "Sept 2022")),       ### to add selected country
    
    
    mainPanel(gt_output(outputId = "climateFinanceRatings1"),
              gt_output(outputId = "climateFinanceRatings2"))
  ),
  wellPanel(  ## 3 exporting steps
    p("Export temporary files anywhere inside defined path in the script"),
    p("4. Export top half of table"),  
    downloadButton("download1", "Download top"),
    p(""),
    p(""),
    p("5. Export bottom half of table"), 
    downloadButton("download2", "Download bottom"),
    p(""),
    p(""),
    p("6. Export merged table as pdf"), 
    downloadButton("downloadFinal", "Download final.pdf"))
)