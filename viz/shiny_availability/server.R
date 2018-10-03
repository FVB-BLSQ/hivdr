

  function(input, output) {
    
    output$plot1 <- renderPlot({
        
        m <- data %>%
          filter(
            DS_name == input$dataset,
            level_2_name == input$region)
        m <- as.data.frame(m)

      ggplot(m)+
        geom_line(aes(x=variable, y=value, col = DS_name, group=orgUnit), alpha = .3) +
        facet_wrap(level_3_name~DS_name, scales = 'free_y') +
        theme_minimal()+
        guides(color=FALSE)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_x_date(date_minor_breaks = "1 month", date_labels =  "%Y", date_breaks = "1 year") +
        xlab('') + ylab('Time to data entry')
      
    })
    
    
    output$plot2 <- renderPlot({
      
      m <- data_map %>%
        filter(
          dataElement == input$dataelement)
      m <- as.data.frame(m)
      
      
      ggplot(m)+
        geom_polygon(aes(x  = long, y = lat , 
                         group=id, 
                         fill = reporting_rate))+
        geom_polygon(data =  coordinates_region , aes(x  = long, y = lat , 
                                                      group=id), fill=NA, colour='black', size = .2, alpha=.6)  +
        theme_minimal() +
        coord_map() + 
        scale_fill_gradient(name = "Data Element Availability",
                            low = '#ef0404', high = '#6ddb3f',
                            space = "Lab",
                            na.value = "grey50", guide = "colourbar",
                            limits=c(0,1))+
        theme(legend.position = "bottom")
      
    })
  
  }
  
  