## sc_ERP_Sales_sv.R

url_Sales_as_is <- a("Peter github Program",
                   href="https://github.com/plus4u/Data-Analytics/commit/1d1d29928a1021a6ad470f00a0c5c3bdb7372fae") 

output$tab_Sales_1 <- renderUI({   
    tagList("URL link:", url_Sales_as_is)   })  

output$Img_Sales_KPI = renderImage({
    list(src = "Img_Sales_KPI.png")   
}, deleteFile = FALSE)

output$d_Sales_KPI <- renderDataTable({
    #  Data <- the_data()
    Data <- d_Sales_KPI
    if (is.null(Data )){ return(NULL)}
    Data    }) 

output$tab_Sales <- renderPrint({
    str ( the_data() )    })   #    x_var()  

#

output$tab_2_b <- renderDataTable({
    ERP_Sales_DS    }) 

output$tab_2_c <- renderDataTable({ 
    Sales_Gs    }) 

output$tab_2_d <- renderDataTable({  # 1 -----
    ERP_Sales_GS    }) 

output$tab_2_e <- renderDataTable({ 
    Sales_Tot    }) 

output$tab_2_f <- renderDataTable({  #  Sales_fix_f
    Sales_fix_f    }) 

output$tab_2_g <- renderDataTable({  #  
    Sales_fix_nf    }) 

output$tab_2_h <- renderDataTable({  #  
    Inventory    }) 

output$tab_3_a <- renderPrint({
    x_var()     })   #  str ( the_data() )

output$tab_3_b <- renderPrint({ 
    summary ( the_data() )    }) 

output$tab_2_a_2 <- renderDataTable({ 
    ERP_pay   }) 
