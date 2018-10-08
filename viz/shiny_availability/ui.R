
  
  shinyUI(

    mainPanel(
      br(),
      tabsetPanel(
        tabPanel(
          "Data Element",
          fluidPage(
            br(),
            fluidRow(
              
              column(6,
                     selectInput("dataelement", "Data Element", 
                                 choices = unique(de_availability_map$name_data_element), 
                                 selected = unique(de_availability_map$name_data_element)[1]),
                     selectInput("period", "Period", 
                                 choices = unique(de_availability_map$period), 
                                 selected = unique(de_availability_map$period)[-1])
              ),
              column(6,
                     selectInput("region", "Region", 
                                 choices = unique(de_availability_timeline$namelevel2),
                                 selected = unique(de_availability_timeline$namelevel2)[1]),
                     br(),
                     textOutput("print1"),
                     textOutput("print2"),
                      tags$head(tags$style("#print2{color: black;
                                 font-size: 30px;
                                          font-style: bold;
                                          }"))
              )
            ),
            br(),
            fluidRow(
                
              column(6,
                      plotOutput("plot1", height = "600px")
              ),
              column(6,
                      plotOutput("plot2", height = "300px"),
                      DT::dataTableOutput('datatable1', height = "400px")
              )
            )
          )
        )
        
      ), width = 12))
  
  