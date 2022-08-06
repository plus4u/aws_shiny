### aws_shiny_1.R

library(shiny)
library(shinythemes) 
library(shinydashboard) 

library( readxl)  
library(dplyr) # dashboard %>% 
library(ggplot2) # dashboard function ggplot

library(ggvis) # bind_shiny
library(shinyjs) # for image-datatable click

ui <- navbarPage("Data Analytics",  collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"), 
                 
                 Tab_Sales, 
                 navbarMenu("BSC",
                            tabPanel("Dashboard", 
                                     dashboardPage(ds_header, ds_sidebar, ds_body, skin='red')     ), 
                            "--- e2e Process---", 
                            
                            tabPanel("Order 2 Cash",   ),
                            tabPanel("Order 2 Delivery",   ),
                            tabPanel("Purchase 2 Pay",   ),
                            tabPanel("Plan 2 Forecast",   ),
                            tabPanel("Actual 2 Cost",   )  ),
                 Tab_Statistics,
                 Tab_ML, 
)


### server ################ 

server <- function(input, output, session) {
    
    # ERP      # sales    
    pack_UC_sv("Demand_F", the_data)  
    pack_UC_sv_3("Demand_SI_DL", the_data)   
    pack_UC_sv("ERP_ANOVA_o", the_data)  
    pack_UC_sv("ERP_ANOVA_t", the_data)  
    
    pack_UC_sv("CronBA", the_data)  
    pack_UC_sv("BCG_GE", the_data)
    pack_UC_sv("Cost_Model", the_data) 
    pack_UC_sv("BOM_Price", the_data) 
    pack_UC_sv("Chi_Sq", the_data) 
    
    pack_UC_sv("P&L CF", the_data)  
    
    source("sc_ERP_Sales_sv.R",local = TRUE)  
    source("sc_stat_sv_xls_2.R",local = TRUE) 
    source(file="dashboard_sv.R", local=T) 
    
    pack_UC_sv("xor", the_data)  
    pack_UC_sv("Lin_R", the_data)   
    pack_UC_sv("Cla_rg", the_data) 
    pack_UC_sv("Log_rg", the_data)  
    pack_UC_sv("CNN", the_data)   
    pack_UC_sv("RNN", the_data) 
    
    pack_UC_sv("YoLo", the_data)  
    pack_UC_sv("Numpy", the_data)   
    pack_UC_sv("Pandas", the_data) 
    pack_UC_sv("Matplot", the_data)  
    pack_UC_sv("Tensorf", the_data)   
    pack_UC_sv("PyTorch", the_data) 
    pack_UC_sv("SciPy", the_data)  
    
    pack_UC_sv("Anacoda", the_data)   
    pack_UC_sv("Colab", the_data) 
    pack_UC_sv("Jupyter", the_data) 
    
}

######################################
shinyApp(ui, server) 

