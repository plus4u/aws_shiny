
## dashboard_sv.R 
  total.revenue <- sum(recommendation$Revenue)
  prof.account <- recommendation %>% group_by(Account) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
  prof.prod <- recommendation %>% group_by(Product) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
  
  
  
  output$profAcc <- renderValueBox({
    valueBox(
      formatC(prof.account$value, format="d", big.mark=',')
      ,paste('Top Account:',prof.account$Account)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
  })
  
  output$marketShare <- renderValueBox({
    
    valueBox(
      formatC(total.revenue, format="d", big.mark=',')
      ,'Total Expected Revenue'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "green")
  })
  
  output$profitMargin <- renderValueBox({
    valueBox(
      formatC(prof.prod$value, format="d", big.mark=',')
      ,paste('Top Product:',prof.prod$Product)
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")
    
  })
  
  output$salesQuartBar <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=Product, y=Revenue, fill=factor(Region))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
      xlab("Product") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Revenue by Product") + labs(fill = "Region")
  })
  
  output$salesYearBar <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=Account, y=Revenue, fill=factor(Region))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
      xlab("Account") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Revenue by Region") + labs(fill = "Region")
  })
  
  output$shareLine <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=Account, y=Revenue, fill=factor(Product))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
      xlab("Account") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Revenue by Account") + labs(fill = "Product")
    
  })
  
  output$shareBar <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=Region, y=Revenue, fill=factor(Product))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
      xlab("Region") + theme(legend.position="bottom" 
                             ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Revenue by Region") + labs(fill = "Product")
    
  })
  
  output$salesbymodel <- renderDataTable(recommendation %>% group_by(Account) %>% summarize('Expected Revenue' = sum(Revenue)))
  output$salesbyquarter <- renderDataTable(recommendation %>% group_by(Region) %>% summarize('Expected Revenue' = sum(Revenue)))
  output$prioryearsales <- renderDataTable(recommendation %>% group_by(Product) %>% summarize('Expected Revenue' = sum(Revenue)))
  
  output$NameBox <- renderInfoBox({
    infoBox(
      title = "Date"
      ,value = Sys.Date()
      ,color = "purple"
      ,icon = icon("tachometer")
    )
  })
  
  output$NameBox <- renderInfoBox({
    infoBox(
      title = "Date"
      ,value = Sys.Date()
      ,color = "purple"
      ,icon = icon("tachometer")
    )
  })
  