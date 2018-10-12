function(input, output) {
  
  ## SOURCE ######################################################################################################
  
  ## DATA 
  
  df1_region_cordaid_filt <- reactive({
    m <- df1_region_cordaid %>%
      filter(
        periods == input$period)
    m <- left_join(coordinates_region, m, by = c('id' = 'level_2_id'))
    m$value[is.na(m$value)] <- 0
    m
  })
  
  
  df1_region_pnls_filt <- reactive({
    m <- df1_region_pnls %>%
      filter(
        periods == input$period)
    m <- left_join(coordinates_region, m, by = c('id' = 'level_2_id'))
    m$value[is.na(m$value)] <- 0
    m
  })
  
  df1_region_combine_filt <- reactive({
    m <- df1_region_combine %>%
      filter(
        periods == input$period)
    m <- left_join(coordinates_region, m, by = c('id' = 'level_2_id'))
    m$value[is.na(m$value)] <- 0
    m
  })
  
  df1_all_filt <- reactive({
    m <- df1_all %>%
      filter(
        periods == input$period,
        level_2_name == input$region)
    m$pnls[is.na(m$pnls)] <- 0
    m$cordaid[is.na(m$cordaid)] <- 0
    m$combine[is.na(m$combine)] <- 0
    m
  })
  
  df1_region_all_filt <- reactive({
    m <- df1_region_all %>%
      filter(
        periods == input$period)
    m$pnls[is.na(m$pnls)] <- 0
    m$cordaid[is.na(m$cordaid)] <- 0
    m$combine[is.na(m$combine)] <- 0
    m
  })
  
  ## PLOT 
  
  output$plot1 <- renderPlot({
    
    ggplot(df1_region_pnls_filt())+
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
    
    ggplot(df1_region_cordaid_filt())+
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
    
    ggplot(df1_region_combine_filt())+
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
    dttbl <- df1_region_all_filt()
    
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
    dttbl <- df1_all_filt()
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
  
  
  ## PATIENTS ######################################################################################################
  
  ## DATA 
  
  
  df2_region_filt1 <- reactive({
    m <- df2_region %>%
      filter(
        periods == input$period2)
    m <- left_join(coordinates_region, m, by = c('id' = 'level_2_id'))
    m$value[is.na(m$value)] <- 0
    m$all[is.na(m$all)] <- 0
    m
  })
  
  df2_region_filt2 <- reactive({
    m <- df2_region %>%
      filter(
        periods == input$period2,
        level_2_name == input$region2)
    m$value[is.na(m$value)] <- 0
    m <- m %>% filter(value != 0)
    m
  })
  
  
  ## PLOT 
  
  output$plot4 <- renderPlot({
    
    ggplot(df2_region_filt1())+
      geom_polygon(aes(x  = long, y = lat , 
                       group=id, 
                       fill = all))+
      theme_minimal() +
      coord_map() + 
      scale_fill_gradient(name = "Number of patients",
                          low = '#eeeeee', high = '#ef0404',
                          space = "Lab",
                          na.value = "grey50", guide = "colourbar")+
      ggtitle("TOTAL PATIENTS")+
      theme(plot.title = element_text(size=20, hjust = 0.55),
            legend.position = "bottom",
            legend.key.width = unit(2, "cm"),
            legend.text=element_text(size=13),
            legend.title=element_text(size=15))
    
  })
  
  
  output$plot5 <- renderPlot({
    
    df <- df2_region_filt2()
    
    ggplot(df, aes(x="", y=value, fill=line)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y") +
      blank_theme +
      theme(axis.text.x=element_blank(),
            legend.key.width = unit(2, "cm"),
            legend.text=element_text(size=13),
            legend.title=element_text(size=15)) + 
      scale_fill_manual(name = "Treatment", values=group.colors)
    
    
  })
  
  output$print1 <- renderText({
    paste0("Total number of patients for ", input$period2," (", input$region2, ") :")
  })
  
  output$print2 <- renderText({
    m <- df2_region_filt2()
    n_patients <- m$all[1]
    n_patients
  })
  
  
  
  ## FACILITIES ######################################################################################################
  
  ## DATA
  
  df4_region_filt1 <- reactive({
    m <- df4_region %>%
      filter(
        periods == input$period3)
    m <- left_join(coordinates_region, m, by = c('id' = 'level_2_id'))
    m
  })                  
  
  ## DATA
  
  output$plot6 <- renderPlot({
    
    df <- df3_region
    df <- left_join(coordinates_region, df, by = c('id' = 'level_2_id'))
    
    ggplot(df)+
      geom_polygon(aes(x  = long, y = lat ,
                       group=id,
                       fill = value))+
      theme_minimal() +
      coord_map() +
      scale_fill_gradient(name = "Number of facilities",
                          low = '#eeeeee', high = '#4D71A3',
                          space = "Lab",
                          na.value = "grey50", guide = "colourbar")+
      ggtitle("FACILITIES")+
      theme(plot.title = element_text(size=20, hjust = 0.55),
            legend.position = "bottom",
            legend.key.width = unit(2, "cm"),
            legend.text=element_text(size=13),
            legend.title=element_text(size=15))
    
  })
  
  output$plot7 <- renderPlot({
    
    df <- df4_region_filt1()
    
    ggplot(df)+
      geom_polygon(aes(x  = long, y = lat ,
                       group=id,
                       fill = percentage))+
      theme_minimal() +
      coord_map() +
      scale_fill_gradient(name = "% of facilities",
                          low = '#228b22', high = '#ef0404',
                          space = "Lab",
                          na.value = "grey90", guide = "colourbar",
                          limits = c(0,1))+
      ggtitle("% OF FACILITIES WITH STOCKOUTS")+
      theme(plot.title = element_text(size=20, hjust = 0.55),
            legend.position = "bottom",
            legend.key.width = unit(2, "cm"),
            legend.text=element_text(size=13),
            legend.title=element_text(size=15))
    
  })
  
  plot8_select <- reactive({
    m <- df5_region %>%
      filter(
        stand_name == input$stand_name,
        namelevel2 == input$province_conso)
    m
  })  
  
  output$plot8 <- renderPlot({
    
    df <- plot8_select()
    
    
    
    ggplot(df)+
      geom_line(aes(x  = period, y = value))+
      geom_smooth(aes(x  = period, y = value), method='loess')+
      theme_minimal() +
      ggtitle("Consumption Timeline")+
      theme(plot.title = element_text(size=20, hjust = 0.55),
            legend.position = "bottom",
            legend.key.width = unit(2, "cm"),
            legend.text=element_text(size=13),
            legend.title=element_text(size=15))
    
  })
  
  
  
  plot9_select <- reactive({
    m <- df5_region %>%
      filter(
        stand_name == input$stand_name,
        period == input$period_conso)
    m <- left_join(coordinates_region, m, by = c('id' = 'uidlevel2'))
    m
  })  
  
  output$plot9 <- renderPlot({
    
    df <- plot9_select()
    
    ggplot(df)+
      geom_polygon(aes(x  = long, y = lat ,
                       group=id,
                       fill = value))+
      theme_minimal() +
      coord_map() +
      scale_fill_gradient(name = "Number of dispensed boxes",
                          low = '#eeeeee', high = '#ef0404',
                          space = "Lab",
                          na.value = "grey90", guide = "colourbar")+
      ggtitle("Total number of dispensed boxes")+
      theme(plot.title = element_text(size=20, hjust = 0.55),
            legend.position = "bottom",
            legend.key.width = unit(2, "cm"),
            legend.text=element_text(size=13),
            legend.title=element_text(size=15))
    
  })
  
  
  
}