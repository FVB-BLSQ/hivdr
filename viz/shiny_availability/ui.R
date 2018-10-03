
  
  shinyUI(

    mainPanel(
      hr(),
      dateRangeInput('dateRange',
                       label = NULL,
                       start = '2012-02-09',
                       end = Sys.Date() + 2
      ),
      hr(),
      tabsetPanel(
        
        tabPanel(
          "Timeliness",
          fluidPage(
            hr(),
            fluidRow(
              column(6,
                     selectInput("dataset", "Dataset", choices = c(levels(data$DS_name)), c(levels(data$DS_name))[1])
              ),
              column(6,
                     selectInput("region", "Region", choices = c(levels(data$level_2_name)), selected =c(levels(data$level_2_name))[1] )
              )
            ),
            plotOutput("plot1")
          )
        ),
        
        tabPanel(
          "Map",
          fluidPage(
            hr(),
            fluidRow(
              column(6,
                     selectInput("dataelement", "DataElement", choices = c(levels(data_map$dataElement)), c(levels(data_map$dataElement))[1])
              )
            ),
            plotOutput("plot2")
          )
        )
        
      ), width = 12))
  
  