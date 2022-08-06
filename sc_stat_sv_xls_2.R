## sc_stat_sv_xls_2.R

# Render Image File
output$img1 = renderImage({
    list(src = "IIoT_PLM.png", width=1100 )   # shiny.png
}, deleteFile = FALSE)


## welcome ####


### navelist Menu

## tab_us1
# url_1 <- a("Peter github Program", href="https://github.com/plus4u/innovation/blob/master/pi_digital_factory.html")
# output$tab_us1 <- renderUI({   tagList("URL link:", url_1)
# }) 

## tab_us2
# url_2 <- a("Peter github Homepage", href="https://plus4u.github.io/innovation/")
# output$tab_us2 <- renderUI({ tagList("URL link:", url_2)
# })

## tab_us3
# url_3 <- a("Peter Naver Homepage", href="https://blog.naver.com/gwihyeonseo")
# output$tab_us3 <- renderUI({  tagList("URL link:", url_3)
# })

## type-1 #
# data_1 <- reactive({
#    file_q <- input$file_1
#    if(is.null(file_q)){return()} 
#  #  read.csv(file=file_q$datapath, sep=input$sep_q, header = input$header_q,   stringsAsFactors = input$stringsAsFactors_q)  })
#    read_excel( file_q$datapath, col_names =TRUE )  })

## type-2 ##

userFile <- reactive({ 
    validate(need(input$file1, message = FALSE))
    input$file1  })

## end 

output$filedf_q <- renderTable({
    if(is.null(data_1())){return()}
    input$file_1 }) 

output$sum_q <- renderTable({
    if(is.null(data_1())){return()}
    summary(data_1())
})

output$table_q <- renderTable({
    if(is.null(data_1())) {return()}
    data_1()
})


## t-test server start ###########

# data <- reactive({
#    inFile <- input$file1 
#    if (is.null(inFile)){return(NULL)} 
#    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
#              quote=input$quote) })

## type-2 ##

# data <- reactive({
#    inFile <- input$file1 
#    if (is.null(inFile)){return(NULL)} 
#    read_excel( inFile$datapath, col_names =TRUE )  })


data <- reactive({
    # read_excel( userFile$datapath, col_names = input$heading )   # col_names = TRUE, stringsAsFactors = stringsAsFactors
    dataframe <- read_excel( userFile()$datapath, col_names =TRUE )   ## be careful ()
    dataframe <- data.frame(dataframe )
    return(dataframe)    })   

## end

# Updata value user could select
observe({
    updateSelectInput( session,    "var1",  choices=names(data()))
})
# Updata value user could select
observe({
    updateSelectInput( session,  "var2",   choices=names(data()))
})

# Output a t distribution plot
output$tplot <- renderPlot({
    # Display the Student's t distributions with various
    # degrees of freedom and compare to the normal distribution
    x <- seq(input$range[1], input$range[2], length=100)
    hx <- dnorm(x)  
    labels <- c('t distribution', 'normal distribution')
    
    plot(x, hx, type="l", lty=2, xlab="x value", col = "black",
         ylab="Density", main="t Distributions")
    
    lines(x, dt(x,input$df), lwd=2, col="red")
    
    legend("topright", inset=.05, title="Distributions",
           labels, lwd=2, lty=c(1, 2), col=c("red", "black"))
})

# Output a data table for the upload tab page
output$contents_tt <- renderTable({
    inFile <- input$file1 
    if (is.null(inFile))
        return(NULL)
    # read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)  })
    read_excel( inFile$datapath, col_names =TRUE )  })


# Output a histogram for the variables user chose
output$graph <- renderPlot({
    var1 <- data()[,input$var1]
    var2 <- data()[,input$var2]
    if (is.null(var1)){return(NULL)}
    if (is.null(var2)){return(NULL)}
    
    graph2 <- ifelse(input$sample == 'oneSamp', FALSE, TRUE)
    p1 <- hist(var1, breaks = input$bins)
    p2 <- hist(var2, breaks = input$bins)
    plot(p1, col=rgb(0,0,1,1/4))
    
    if(input$sample == 'twoSamp' | input$sample =='Paired')
        plot(p2, col=rgb(1,0,0,1/4),add = graph2)
})

# Output of discriptive summary of this variable
output$disc_tt <-  renderPrint({
    Data <- data()
    if (is.null(Data)){return(NULL)}
    summary(Data)
})

# Output of the data structure
output$str_tt <- renderPrint({
    Data <- data()
    if (is.null(Data)){return(NULL)}
    str(Data)
})

# Create a one sample and two sample t-test reactive function
ttestout <- reactive({
    var1 <- data()[,input$var1]
    conf <- input$conf
    
    if (is.null(var1)){return(NULL)}
    t1 <- t.test(var1, alternative = input$tail, mu = input$test, conf.level = conf)
    var2 <- data()[,input$var2]
    
    if (is.null(var2)){return(NULL)}
    
    ve <- ifelse(input$varequal == 'y', TRUE, FALSE)
    t2 <- t.test(var1, var2, alternative = input$tail, var.equal = ve, conf.level = conf)
    t3 <- t.test(var1, var2, paired = TRUE, alternative = input$tail, var.equal = ve, conf.level = conf)
    
    if(input$sample == "oneSamp") {return(t1)}
    
    if(input$sample == "twoSamp") {return(t2)}
    
    if(input$sample == "Paired") {return(t3)}
    
})

# Output of one sample t value of t-test
output$tvalue <- renderPrint({
    vals <- ttestout()
    if (is.null(vals)){return(NULL)}
    vals$statistic
})

# Output of p value
output$pvalue <- renderPrint({
    vals <- ttestout()
    if (is.null(vals)){return(NULL)}
    vals$p.value 
})

# Output of key statistical parametric
output$parametric <- renderTable({
    var1 <- data()[,input$var1]
    if (is.null(var)){return(NULL)}
    var2 <- data()[,input$var2]
    if (is.null(var)){return(NULL)}
    mean1 <- mean(var1)
    mean2 <- mean(var2)
    standard_deviation1 <- sd(var1)
    standard_deviation2 <- sd(var2)
    standard_error1 <- sd(var1)/sqrt(length(var1))
    standard_error2 <- sd(var2)/sqrt(length(var2))
    parametric1 <- data.frame(mean = mean1, 
                              standard_deviation=standard_deviation1, 
                              standard_error=standard_error1)
    rownames(parametric1) <- input$var1
    parametric2 <- data.frame(mean = mean2, 
                              standard_deviation=standard_deviation2, 
                              standard_error=standard_error2)
    rownames(parametric2) <- input$var2
    if(input$sample == "oneSamp") {return(parametric1)}
    if(input$sample == "twoSamp") {return(rbind(parametric1,parametric2))}
})

## anova server start ######

data_2 <- reactive({
    inFile_2 <- input$file_2 
    if (is.null(inFile_2)){return(NULL)} 
    read.csv(inFile_2$datapath, header=input$header_2, sep=input$sep_2,   quote=input$quote_2)  })
#  read_excel( inFile_2$datapath, col_names =TRUE )  }) # check not working
# read_excel( inFile_2()$datapath, col_names =TRUE )  })

# Updata value user could select
observe({
    updateSelectInput(
        session,
        "var2_1",
        choices=names(data_2()))
    
})
# Updata value user could select
observe({
    updateSelectInput(
        session,
        "var2_2",
        choices=names(data_2()))
    
})

# Output a t distribution plot
output$tplot_2 <- renderPlot({
    # Display the Student's t distributions with various
    # degrees of freedom and compare to the normal distribution
    
    x <- seq(input$range_2[1], input$range_2[2], length=100)
    hx <- dnorm(x)  
    labels <- c('t distribution', 'normal distribution')
    
    plot(x, hx, type="l", lty=2, xlab="x value", col = "black",
         ylab="Density", main="t Distributions")
    
    
    lines(x, dt(x,input$df_2), lwd=2, col="red")
    
    
    legend("topright", inset=.05, title="Distributions",
           labels, lwd=2, lty=c(1, 2), col=c("red", "black"))
})

# Output a data table for the upload tab page
output$contents_2 <- renderTable({
    inFile_2 <- input$file_2 
    if (is.null(inFile_2))
        return(NULL)
    read.csv(inFile_2$datapath, header=input$header_2, sep=input$sep_2, quote=input$quote_2)  })
#  read_excel( inFile_2$datapath, col_names =TRUE )  })

# Output a histogram for the variables user chose
output$graph_2 <- renderPlot({
    var2_1 <- data_2()[, input$var2_1]
    var2_2 <- data_2()[, input$var2_2]
    if (is.null(var2_1)){return(NULL)}
    if (is.null(var2_2)){return(NULL)}
    
    #        graph2 <- ifelse(input$sample == 'oneSamp', FALSE, TRUE)
    
    p1 <- hist(var2_1, breaks = input$bins_2)
    p2 <- hist(var2_2, breaks = input$bins_2)
    plot(p1, col=rgb(0,0,1,1/4))
    #        plot(p2, col=rgb(1,0,0,1/4),add = graph2)
    plot(p2, col=rgb(1,0,0,1/4))
})

# Output of discriptive summary of this variable
output$disc_2 <-  renderPrint({
    Data_2 <- data_2()
    if (is.null(Data_2 )){return(NULL)}
    summary(Data_2 )
})

# Output of the data structure
output$str <- renderPrint({
    Data_2 <- data_2()
    if (is.null(Data_2 )){return(NULL)}
    str(Data_2 )
})

# Create a one sample and two sample anova reactive function

ttestout_2 <- reactive({
    var2_1 <- data_2()[, input$var2_1]
    var2_2 <- data_2()[, input$var2_2 ]
    conf_2 <- input$conf_2
    #       if (is.null(var1)){return(NULL)} 
    
    ve <- ifelse(input$varequal == 'y', TRUE, FALSE)
    #        t2 <- aov(var1 ~ var2, alternative = input$tail, var.equal = ve, conf.level = conf)
    t2 <- aov(var2_1 ~ var2_2)
    return(t2) 
    
})

# Output of p value of anova
output$pvalue_2 <- renderPrint({
    vals_2 <- ttestout_2()
    summary(vals_2)
})


# Output of key statistical parametric
output$parametric_2 <- renderTable({
    var2_1 <- data_2()[, input$var2_1]
    if (is.null(var2_1)){return(NULL)}
    var2_2 <- data_2()[, input$var2_2]
    if (is.null(var2_2)){return(NULL)}
    mean1 <- mean(var2_1)
    mean2 <- mean(var2_2)
    standard_deviation1 <- sd(var2_1)
    standard_deviation2 <- sd(var2_2)
    standard_error1 <- sd(var2_1)/sqrt(length(var2_1 ))
    standard_error2 <- sd(var2_2)/sqrt(length(var2_2 ))
    parametric2_1 <- data.frame(mean = mean1, 
                                standard_deviation=standard_deviation1, 
                                standard_error=standard_error1)
    rownames(parametric2_1) <- input$var2_1
    parametric2_2 <- data.frame(mean = mean2, 
                                standard_deviation=standard_deviation2, 
                                standard_error=standard_error2)
    rownames(parametric2_2) <- input$var2_2
    return(rbind(parametric2_1,parametric2_2))
})

## anova end ####

## chi square, F test

pack_UC_sv("Chi-Square", the_data)  

pack_UC_sv("F Test_all", the_data) 

## tab_7 ##########

# url_72 <- a("Peter colab ", href="https://colab.research.google.com/drive/1vwihV30lrkM7Uo9RTDSvQyeM6pqE8F1T")
# output$tab_us72 <- renderUI({  tagList("URL link:", url_72)
# })

## linear regression ####

data_lr <- reactive({
    file1 <- input$file_lr
    if(is.null(file1)){return()} 
    
    read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    
})  

output$var_y <- renderUI({
    checkboxGroupInput("var_y_select","Select respond Var", choices =as.list(names(data_lr() )) )
})

output$var_x <- renderUI({
    checkboxGroupInput("var_x_select","Select independant Var", choices =as.list(names(data_lr())))
})  

# Regression output
output$summary <- renderPrint({
    fit <- lm(data_lr()[,input$var_y_select] ~ data_lr()[,input$var_x_select])
    names(fit$coefficients) <- c("Intercept", input$var2)
    summary(fit)
})

# Data output
output$tbl = DT::renderDataTable({
    DT::datatable(data_lr(), options = list(lengthChange = FALSE))
})


# Scatterplot output
output$scatterplot <- renderPlot({
    plot(data_lr()[,input$var_x_select], data_lr()[,input$var_y_select], main="Scatterplot",
         xlab=input$var_x_select, ylab=input$var_y_select, pch=19)
    abline(lm(data_lr()[,input$var_x_select] ~ data_lr()[,input$var_y_select]), col="red")
    lines(lowess(data_lr()[,input$var_y_select],data_lr()[,input$var_x_select]), col="blue")
}, height=400)


# Histogram output var 1
output$distribution1 <- renderPlot({
    hist(data_lr()[,input$var_x_select], main="", xlab=input$var_x_select)
}, height=300, width=300)

# Histogram output var 2
output$distribution2 <- renderPlot({
    hist(data_lr()[,input$var_y_select], main="", xlab=input$var_y_select)
}, height=300, width=300)



## logistic server ####

data_logit <- reactive({
    inFile <- input$file_logit
    if(is.null( inFile )){return()} 
    # read_xls( inFile$datapath, col_names = TRUE ) 
    read_excel( inFile$datapath, col_names = TRUE ) 
})

observeEvent(data_logit(), {
    updateSelectInput(session, "col", choices = names(data_logit()))
})

#  

output$tb_logit <- renderUI ({
    if(is.null(data_logit())) {return()} 
    else
        tabsetPanel(
            tabPanel('About logistic',   
                     fluidRow(column(10, offset = 1, 
                                     h3("t Distribute Understanding"),  plotOutput('logit_plot') ,
                                     h3("logistic concept explanation : github contents"),
                                     mainPanel(  uiOutput("tab_2_1")  )   ) ) ),
            
            tabPanel('Input Data View', 
                     fluidRow(column(10, offset = 1, h3("Data Structure"), verbatimTextOutput('logit_str'))),
                     fluidRow(column(10, offset = 1, 
                                     h3("Data Summary"),  verbatimTextOutput('logit_disc') ,
                                     h3("Data Table"), tableOutput('logit_contents')) ) ) ,     
            
            tabPanel('glm',
                     fluidRow(column(8, offset = 1,
                                     h3("Key summary statistics"),
                                     p("The observed sample statistics were:"),
                                     verbatimTextOutput('glm_sum'),
                                     
                                     h3("Hypothesis of the glm"),
                                     p("We are testing the null hypothesis that ~"),
                                     p("The observed glm :"),
                                     tableOutput('fitted_con'),
                                     
                                     p(" is corrected value."),
                                     textOutput('cor_value')))
            ),
            tabPanel('ROC',
                     
                     fluidRow(column(8, offset = 1,
                                     h3("Key summary statistics"),
                                     p("The observed sample statistics were:"),
                                     plotOutput('rplot'),
                                     
                                     h3("ROC summary"), 
                                     p("ROC :"),
                                     verbatimTextOutput('r_disc'),
                                     
                                     p("Cut-off value"),
                                     textOutput('cut_value')))
            ))
})


# Output a t distribution plot
output$logit_plot <- renderPlot({
    
    x <- seq(input$range[1], input$range[2], length=100)
    
    hx <- dnorm(x)  
    labels <- c('t distribution', 'normal distribution')
    plot(x, hx, type="l", lty=2, xlab="x value", col = "black",
         ylab="Density", main="t Distributions")
    lines(x, dt(x,input$df), lwd=2, col="red")
    legend("topright", inset=.05, title="Distributions",
           labels, lwd=2, lty=c(1, 2), col=c("red", "black"))
})

##  
# url_2 <- a("Peter github Program", href="https://github.com/plus4u/Data-Analytics/blob/master/%231-mes-1.md") 
# output$tab_2_1 <- renderUI({   tagList("URL link:", url_2)
# }) 

# Output of the data structure
output$logit_str <- renderPrint({
    Data <- data_logit()
    if (is.null(Data)){return(NULL)}
    str(Data)
})

# Output of discriptive summary of this variable
output$logit_disc <-  renderPrint({
    Data <- data_logit()
    if (is.null(Data)){return(NULL)}
    summary(Data)
})

# Output a data table for the upload tab page
output$logit_contents <- renderTable({
    Data <- data_logit()
    inFile <- input$file_logit 
    if (is.null(inFile))
        return(NULL)
    # read_xls( inFile$datapath, col_names = TRUE )  
    head( Data)
    
})

# glm output

glmout <- reactive({
    myd2 <- data_logit()
    # var1 <- data()[,input$var1]
    # conf <- input$conf
    if (is.null( myd2)){return(NULL)}
    # t1 <- t.test(var1, alternative = input$tail, mu = input$test, conf.level = conf)
    m1 <- glm( P_F ~ x1 + x2 + x3 + x4 , data= myd2, family = binomial)
    return(m1)  
})

# Output of value of glm
output$glm_sum <- renderPrint({
    vals <- glmout()
    if (is.null(vals)){return(NULL)}
    summary( vals)
    # v <- summary( vals)
    # v$coefficients 
    # tab_model( vals)
})

fittedout <- reactive({
    myd2 <- data_logit()
    # var1 <- data()[,input$var1]
    # conf <- input$conf
    if (is.null( myd2 )){return(NULL)} 
    m1 <- glm( P_F ~ x1 + x2 + x3 + x4 , data= myd2, family = binomial)
    logit_f <- fitted( m1)
    return(logit_f)  
})


# Output of fitted
output$fitted_con <- renderTable({
    vals <- fittedout()
    if (is.null(vals)){return(NULL)}
    table ( round(vals) )
})

# Output of is_corrected
output$cor_value <- renderPrint ({
    myd2 <- data_logit()
    vals <- fittedout()
    if (is.null(vals)){return( NULL)}
    is_correct <- ifelse( vals >.5, 1,0) == myd2$P_F 
    sum(is_correct)
})

# Output of ROC graph 
output$rplot <- renderPlot({ 
    myd2 <- data_logit()  
    labels <- c('t distribution', 'normal distribution')
    ROC(form = P_F ~ x1 + x2 + x3 + x4 , data= myd2, plot="ROC")
    
})

# Output of is_corrected
output$cut_value <- renderPrint ({
    myd2 <- data_logit()
    vals <- fittedout()
    if (is.null(vals)){return( NULL)}
    is_correct <- ifelse( vals >.5, 1,0) == myd2$P_F  
    sum(is_correct)/ NROW(is_correct)
})

# pack_UC_sv("ggvis")

mtcars %>%
    ggvis(~wt, ~mpg) %>%
    layer_points() %>%
    layer_smooths(span = input_slider(0, 1)) %>%
    bind_shiny("ggvis_p", "ggvis_p_ui")


