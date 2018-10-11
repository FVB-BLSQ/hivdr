
  
  shinyUI(

    mainPanel(
      br(),
      tabsetPanel(
        
        ## PANEL 1
        tabPanel(
          "Source",
          fluidPage(
            br(),
            fluidRow(
              
              column(6,
                     selectInput("period", "Period", 
                                 choices = unique(df1$period), 
                                 selected = unique(df1$period)[-1])
              )
            ),
            br(),
            fluidRow(
                
              column(4,
                      plotOutput("plot1", height = "600px")),
              column(4,
                      plotOutput("plot2", height = "600px")),
              column(4,
                      plotOutput("plot3", height = "600px"))
            ),
            br(),
            fluidRow(
              DT::dataTableOutput('datatable1', height = "400px")
            ),
            br(),
            fluidRow(
              column(6,
                     selectInput("region", "Region", 
                                 choices = unique(df1$level_2_name), 
                                 selected = unique(df1$level_2_name)[1])
              )
            ),
            br(),
            fluidRow(
              DT::dataTableOutput('datatable2', height = "400px")
            )
          )
        )
        
        # ## PANEL 2
        # tabPanel(
        #   "Patients",
        #   fluidPage(
        #     br(),
        #     fluidRow(
        #       
        #       column(6,
        #              selectInput("period", "Period", 
        #                          choices = unique(df1$period), 
        #                          selected = unique(df1$period)[-1])
        #       )
        #     ),
        #     br(),
        #     fluidRow(
        #       
        #       column(4,
        #              plotOutput("plot1", height = "600px")),
        #       column(4,
        #              plotOutput("plot2", height = "600px")),
        #       column(4,
        #              plotOutput("plot3", height = "600px"))
        #     ),
        #     br(),
        #     fluidRow(
        #       DT::dataTableOutput('datatable1', height = "400px")
        #     )
        #   )
        # ) 
        
        
      ), width = 12))
  
