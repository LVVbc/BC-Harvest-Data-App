library(shiny)
fluidPage(
  titlePanel("BC Big Game Harvest"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "speciesInput", label = "Species", 
                  choices = c("Black Bear", "Grizzly Bear", "Caribou", "Cougar", "Elk",
                              "Mountain Goat", "Moose", "Sheep", "Wolf", "Mule Deer",
                              "White-tailed Deer", "All Species"), multiple = T, 
                  selected = "Elk"),
      selectInput(inputId = "regionInput", label = "Region", multiple = T,
                  choices = c("All", "1", "2", "3", "4", "5","6", "7A", "7B", "8"), 
                  selected = "All"),
#      selectInput(inputId = "subregionInput", label = "Subregion", multiple = F,
#                  choices = c("", "West Kootenays", "East Kootenays"), selected = ""),
      textInput(inputId = "unitsInput", label = "MUs", value = ""),
      helpText("Optional, overrides Region selction. Enter as whole numbers (e.g. 408) separated by commas"),
      textInput(inputId = "labelInput", label = "Area Name (Optional)",
                value = NULL),
      sliderInput(inputId = "yearsInput", label = "Years", min = 1976, max = 2018,
                  value = c(1972, 2018), sep = ""),
      checkboxInput(inputId = "smoothInput", label = "Smoothed (3 Year Moving Average)"),
      selectInput(inputId = "sexDisplay", label = "Harvest Composition Figure",
                  multiple = F, choices = c("Harvest Composition", "Female Ratio"),
                  selected = "Harvest Composition"),
      selectInput(inputId = "effortInput", label = "Hunter Success Figure",
                  choices = c("Days Per Harvest", "Hunter Success Rate"),
                  multiple = F, selected = "Days Per Harvest"),
      selectInput(inputId = "plotInput", label = "Plot View",
                  choices = c("Main Plot (4 Panels)", "Harvest", "Harvest Composition", 
                              "Female Ratio", "Hunter Success/Days Per Harvest", 
                              "Number of Hunters"), multiple = F, 
                  selected = c("Main Plot (4 Panels")),
      downloadButton("downloadData", "Download Table"),
      width = 3
    ),
    mainPanel(
      plotOutput("coolplot", height = "700px"),
      helpText("Be careful with interpreting the number of hunters. This is only accurate as a statistic
          for a single species at the scale of a single MU, region, or the whole province.
          It may be useful for analyzing trends in other circumstances, but interpret with caution."),
      DT:: dataTableOutput("table")
      )
    )
  
)
