

  function(input, output) {
    
    ######################################################################################################
    
    ## Source panel
    
    data_df1_region_cordaid_filt <- reactive({
      m <- df1_region_cordaid %>%
        filter(
          periods == input$period)
      m <- left_join(coordinates_region, m, by = c('id' = 'level_2_id'))
      m$value[is.na(m$value)] <- 0
      m
    })
      
      
    data_df1_region_pnls_filt <- reactive({
      m <- df1_region_pnls %>%
        filter(
          periods == input$period)
      m <- left_join(coordinates_region, m, by = c('id' = 'level_2_id'))
      m$value[is.na(m$value)] <- 0
      m
    })
    
    data_df1_region_combine_filt <- reactive({
      m <- df1_region_combine %>%
        filter(
          periods == input$period)
      m <- left_join(coordinates_region, m, by = c('id' = 'level_2_id'))
      m$value[is.na(m$value)] <- 0
      m
    })
    
    data_df1_all_filt <- reactive({
      m <- df1_all %>%
        filter(
          periods == input$period,
          level_2_name == input$region)
      m$pnls[is.na(m$pnls)] <- 0
      m$cordaid[is.na(m$cordaid)] <- 0
      m$combine[is.na(m$combine)] <- 0
      m
    })
    
    data_df1_region_all_filt <- reactive({
      m <- df1_region_all %>%
        filter(
          periods == input$period)
      m$pnls[is.na(m$pnls)] <- 0
      m$cordaid[is.na(m$cordaid)] <- 0
      m$combine[is.na(m$combine)] <- 0
      m
    })
    
    
    ######################################################################################################
    
    ## Plot
    
    output$plot1 <- renderPlot({

      ggplot(data_df1_region_pnls_filt())+
        geom_polygon(aes(x  = long, y = lat , 
                         group=id, 
                         fill = value))+
        theme_minimal() +
        coord_map() + 
        scale_fill_gradient(name = "Number of patients",
                            low = '#eeeeee', high = '#ef0404',
                            space = "Lab",
                            na.value = "grey50", guide = "colourbar")+
        ggtitle("PNLS")+
        theme(plot.title = element_text(size=20, hjust = 0.55),
              legend.position = "bottom",
              legend.key.width = unit(2, "cm"),
              legend.text=element_text(size=13),
              legend.title=element_text(size=15))
      
    })
    
    output$plot2 <- renderPlot({
      
      ggplot(data_df1_region_cordaid_filt())+
        geom_polygon(aes(x  = long, y = lat , 
                         group=id, 
                         fill = value))+
        theme_minimal() +
        coord_map() + 
        scale_fill_gradient(name = "Number of patients",
                            low = '#eeeeee', high = '#ef0404',
                            space = "Lab",
                            na.value = "grey50", guide = "colourbar") +
        ggtitle("CORDAID")+
        theme(plot.title = element_text(size=20, hjust = 0.55),
              legend.position = "bottom",
              legend.key.width = unit(2, "cm"),
              legend.text=element_text(size=13),
              legend.title=element_text(size=15))
      
    })
    
    output$plot3 <- renderPlot({
      
      ggplot(data_df1_region_combine_filt())+
        geom_polygon(aes(x  = long, y = lat , 
                         group=id, 
                         fill = value))+
        theme_minimal() +
        coord_map() + 
        scale_fill_gradient(name = "Number of patients",
                            low = '#eeeeee', high = '#ef0404',
                            space = "Lab",
                            na.value = "grey50", guide = "colourbar")+
        ggtitle("COMBINED")+
        theme(plot.title = element_text(size=20, hjust = 0.55),
              legend.position = "bottom",
              legend.key.width = unit(2, "cm"),
              legend.text=element_text(size=13),
              legend.title=element_text(size=15))
      
    })
    
    output$datatable1 <- DT::renderDataTable({
      dttbl <- data_df1_region_all_filt()
      
      DT::datatable(
        dttbl, filter="top", selection="multiple", escape=FALSE,
        rownames = FALSE,
        colnames = c("PROVINCE", "PERIOD", "PNLS", "CORDAID", "COMBINED"),
        options = list(
          dom = 'tp',
          pageLength = 20
        )
      )
    })
    
    output$datatable2 <- DT::renderDataTable({
      dttbl <- data_df1_all_filt()
      dttbl <- dttbl %>% select(-level_2_name)
      
      DT::datatable(
        dttbl, filter="top", selection="multiple", escape=FALSE,
        rownames = FALSE,
        colnames = c("DISTRICT", "PERIOD", "PNLS", "CORDAID", "COMBINED"),
        options = list(
          dom = 'tp',
          pageLength = 20
        )
      )
    })
    
    
    ######################################################################################################
    
    
    
    

  
  #   output$print1 <- renderText({
  #     paste0("Disponibilité du Data Element pour ", input$period," (", input$region, ") :")
  #   })
  #   
  #   output$print2 <- renderText({
  #     m <- data_filt4()
  #     completeness <- m$value * 100
  #     paste0(completeness, "%")
  #   })

  }
  
  