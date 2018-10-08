

  function(input, output) {
    
    data_filt1 <- reactive({
      m <- de_availability_map %>%
        filter(
          name_data_element == input$dataelement,
          period == input$period)
      m <- right_join(coordinates_region, m, by = c('id' = 'uidlevel2'))
      m
    })
    
    data_filt2 <- reactive({
      m <- de_availability_timeline %>%
        filter(
          name_data_element == input$dataelement)
      m
    })
    
    data_filt3 <- reactive({
      m <- de_availability_table %>%
        filter(
          name_data_element == input$dataelement,
          period == input$period,
          namelevel2 == input$region) %>%
        select(namelevel3, value)
      m
    })
    
    data_filt4 <- reactive({
      m <- de_availability_map %>%
        filter(
          name_data_element == input$dataelement,
          period == input$period,
          namelevel2 == input$region)
      m
    })
    
    
    
    output$plot1 <- renderPlot({

      ggplot(data_filt1())+
        geom_polygon(aes(x  = long, y = lat , 
                         group=id, 
                         fill = value))+
        theme_minimal() +
        coord_map() + 
        scale_fill_gradient(name = "Data Element Availability",
                            low = '#ef0404', high = '#6ddb3f',
                            space = "Lab",
                            na.value = "grey50", guide = "colourbar",
                            limits=c(0,1))+
        theme(legend.position = "bottom")
      
    })
    
    
    output$plot2 <- renderPlot({
      
      ggplot(data_filt2())+
        geom_line(aes(x=period, y=value, group = uidlevel2), alpha = 0.2) +
        geom_line(data = subset(data_filt2(),namelevel2==input$region), aes(x=period, y=value, col = uidlevel2, group = uidlevel2), alpha = 1, size = 1.5) + 
        theme_minimal()+
        guides(color=FALSE)+ 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        xlab('') + ylab('Time to data entry')
      
      
    })
    
    output$datatable1 <- DT::renderDataTable({
      dttbl <- data_filt3()
      dttbl <- as.data.frame(dttbl)
      dttbl$value <- dttbl$value * 100
      dttbl_top <- dttbl %>% arrange(-value)

      DT::datatable(
        dttbl_top , filter="top", selection="multiple", escape=FALSE,
        rownames = FALSE,
        colnames = c("Zone de Santé", "Disponibilité (%)"),
        options = list(
          dom = 'tp',
          pageLength = 5
        )
      )
    })
  
    output$print1 <- renderText({
      paste0("Disponibilité du Data Element pour ", input$period," (", input$region, ") :")
    })
    
    output$print2 <- renderText({
      m <- data_filt4()
      completeness <- m$value * 100
      paste0(completeness, "%")
    })
  
  }
  
  