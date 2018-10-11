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
      ),
      
      ## PANEL 2
      tabPanel(
        "Patients",
        fluidPage(
          br(),
          fluidRow(
            
            column(6,
                   selectInput("period2", "Period",
                               choices = unique(df2_region$period),
                               selected = unique(df2_region$period)[-1])
            ),
            column(6,
                   selectInput("region2", "Region",
                               choices = unique(df2_region$level_2_name),
                               selected = unique(df2_region$level_2_name)[1])
            )
          ),
          br(),
          br(),
          fluidRow(
            
            column(6,
                   plotOutput("plot4", height = "600px")),
            column(6,
                   textOutput("print1"),
                   textOutput("print2"),
                   tags$head(tags$style("#print2{color: black;
                                          font-size: 30px;
                                          font-style: bold;
                                          }")),
                   plotOutput("plot5", height = "600px"))
          )
        )
      ),
      
      
      ## PANEL 3
      tabPanel(
        "Facilities",
        fluidPage(
          br(),
          fluidRow(
            column(6,
                   plotOutput("plot6", height = "600px")),
            column(6,
                   selectInput("period3", "Period",
                               choices = unique(df4_region$period),
                               selected = unique(df4_region$period)[-1]),
                   plotOutput("plot7", height = "600px"))
          )
        )
      ),
      
      ## PANEL 4
      tabPanel(
        "Drugs",
        fluidPage(
          br(),
          fluidRow(column(4,selectInput("period_conso", "Period",
                                        choices = sort(unique(df5_region$period)),
                                        selected = sort(unique(df5_region$period))[1])
                          ),
                   column(4, selectInput("stand_name", "Drug",
                               choices = sort(unique(df5_region$stand_name)),
                               selected = 'tdf/3tc/efv(300/300/600 mg) - 30 ces')
                          ),
                   column(4,selectInput("province_conso", "Province",
                               choices = sort(unique(df5_region$namelevel2)),
                               selected = sort(unique(df5_region$namelevel2))[1])
                          )
                   ),
          fluidRow(
            column(6,
                   plotOutput("plot9", height = "600px")),
            column(6,
                   plotOutput("plot8", height = "600px"))
          )
        )
      )
      
      
    ), width = 12))

