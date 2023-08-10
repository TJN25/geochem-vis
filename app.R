library(shiny)
library(tidyverse)
library(DT)
library(ggpubr)


# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Geochem data visualisation"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          conditionalPanel(condition = "input.tabselected==1",
                           uiOutput('select_locations_scatter_output'),
                           uiOutput('select_a_scatter_output'),
                           uiOutput('select_b_scatter_output'),
                           uiOutput('select_c_scatter_output'),
                           uiOutput('select_d_scatter_output'),
                           checkboxInput(inputId = "scatter_show_trend", label = "Show trend line", value = F),
                           checkboxInput(inputId = "scatter_show_legend", label = "Show legend", value = F),
                           radioButtons(inputId = 'scatter_radio_x', label = 'Select X-axis behaviour',choices = c("+", "/"), selected = "/"),
                           radioButtons(inputId = 'scatter_radio_y', label = 'Select Y-axis behaviour',choices = c("+", "/"), selected = "/"),
                           uiOutput(outputId = 'scatter_help_text'),
                           helpText("Leave 'B' and/or 'D' blank to produce plots without a ratio"),
                           sliderInput(inputId = "plot_size_scatter", label = "Plot height", min = 100, max = 1500, value = 400, step = 50),
                           sliderInput(inputId = "scatter_size", label = "Point size", min = 0.5, max = 10, value = 2, step = 0.5),
                           uiOutput('scatter_x_output'),
                           uiOutput('scatter_y_output'),
                           uiOutput('exclude_samples_scatter_output'),
                           uiOutput('exclude_sources_scatter_output')
          ),
          conditionalPanel(condition = "input.tabselected==2",
                           uiOutput('select_locations_spider_output'),
                           uiOutput('exclude_samples_spider_output'),
                           uiOutput('exclude_sources_spider_output'),
                           sliderInput(inputId = "plot_size_spider", label = "Plot height", min = 100, max = 1500, value = 400, step = 50)
          ),
          conditionalPanel(condition = "input.tabselected==3",
                           uiOutput('select_locations_chond_output'),
                           uiOutput('exclude_samples_chond_output'),
                           uiOutput('exclude_sources_chond_output'),
                           sliderInput(inputId = "plot_size_chond", label = "Plot height", min = 100, max = 1500, value = 400, step = 50),
                           checkboxInput(inputId = 'chond_remove_zero', label = "Remove the zero values", value = F)
          ),           
          conditionalPanel(condition = "input.tabselected==4",
                                        uiOutput('select_locations_multiplot_output'),
                                        uiOutput('select_a_multiplot_output'),
                                        uiOutput('select_b_multiplot_output'),
                                        uiOutput('select_c_multiplot_output'),
                                        helpText("This will produce a scatter plot for A/B ~ C(Multiple)"),
                                        helpText("Leave 'B' blank to produce plots without a ratio"),
                                        sliderInput(inputId = "plot_size", label = "Plot height", min = 100, max = 1500, value = 400, step = 50),
                                        uiOutput('exclude_samples_multiplot_output'),
                                        uiOutput('exclude_sources_multiplot_output')
          ),
          conditionalPanel(condition = "input.tabselected==5",
                           uiOutput('select_locations_data_output'),
                           uiOutput('exclude_samples_data_output'),
                           uiOutput('exclude_sources_data_output')
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          
           tabsetPanel(type = "tab", 
                       tabPanel("Scatter Plot",value = 1,
                                uiOutput("scatter_plot_render"),
                                helpText('Click the points to see location and sample.'), 
                                textOutput(outputId = 'scatter_value'), 
                                textOutput(outputId = 'scatter_trend_output'),
                                dataTableOutput('scatter_table')), 
                       tabPanel("Spider Plot",value = 2, 
                                uiOutput("spider_plot_render"),
                                helpText('Click the points to see location and sample.'), 
                                textOutput(outputId = 'spider_value'), 
                                dataTableOutput('spider_table')), 
                       tabPanel("REE Plot",value = 3, 
                                uiOutput("chond_plot_render"),
                                helpText('Click the points to see location and sample.'), 
                                textOutput(outputId = 'chond_value'),
                                dataTableOutput('chond_table')), 
                       tabPanel("Multi Plot",value = 4,
                                uiOutput(outputId = 'multiplot_render'),
                                helpText('Click the points to see location and sample.'), 
                                textOutput(outputId = 'multiplot_value'),
                                dataTableOutput('multiplot_table')), 
                       tabPanel("Data table",value = 5,
                                dataTableOutput('all_data_table')), 
                       id = "tabselected"
                       
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  geochemDat <-  reactive({
      geochemDat <- read.csv('geochem.csv')
      geochemDat
    })


# Scatter -----------------------------------------------------------------

output$select_a_scatter_output <- renderUI({
  selectInput("select_a_scatter", label = "Select column for 'A':", choices = colnames(geochemDat()[5:ncol(geochemDat())]), multiple = F)
}) 

output$select_b_scatter_output <- renderUI({
  selectInput("select_b_scatter", label = "Select column for 'B':", choices = c("-", colnames(geochemDat()[5:ncol(geochemDat())])), selected = "-", multiple = F)
})  

output$select_c_scatter_output <- renderUI({
  selectInput("select_c_scatter", label = "Select column for 'C':", choices = colnames(geochemDat()[5:ncol(geochemDat())]), multiple = F)
})  

output$select_d_scatter_output <- renderUI({
  selectInput("select_d_scatter", label = "Select column for 'D':", choices = c("-", colnames(geochemDat()[5:ncol(geochemDat())])), selected = "-", multiple = F)
})  

output$select_locations_scatter_output <- renderUI({
  selectInput("select_locations_scatter", label = 'Select Locations:', 
              choices = c("-", unique(geochemDat()$Locality)), multiple = T, 
              selected = '-')
})

output$exclude_samples_scatter_output <- renderUI({
  selectInput("exclude_samples_scatter", label = 'Exclude Samples:', 
              choices = c("-", unique(geochemDat()$Sample.ID)), multiple = T, 
              selected = '-')
})

output$exclude_sources_scatter_output <- renderUI({
  selectInput("exclude_sources_scatter", label = 'Exclude Sources:', 
              choices = c("-", unique(geochemDat()$Source)), multiple = T, 
              selected = '-')
})

output$scatter_help_text <- renderUI({
  if(input$select_b_scatter == "-"){
    x_val <- "A"
  }else{
    if(input$scatter_radio_x == "/"){
      x_val <- "A/B"
    }else{
      x_val <- "A+B"
    }
  }
  
  if(input$select_d_scatter == "-"){
    y_val <- "C"
  }else{
    if(input$scatter_radio_y == "/"){
      y_val <- "C/D"
    }else{
      y_val <- "C+D"
    }
  }
  
  helpText(paste0("This will produce a scatter plot for ",  x_val, " ~ ", y_val, "."))
  
})

output$scatter_x_output <- renderUI({
  values <- as.numeric(plotDat()$x)
  values = values[!is.na(values)]
  max_val = max(values)
  min_val = min(values)
  sliderInput(inputId = "scatter_x", min = 0, max = ceiling(max_val), label = "X-axis range", 
              value = c(floor(min_val), ceiling(max_val)), step = 1)
  
})

output$scatter_y_output <- renderUI({
  values <- as.numeric(plotDat()$y)
  values = values[!is.na(values)]
  max_val = max(values)
  min_val = min(values)
  sliderInput(inputId = "scatter_y", min = 0, max = ceiling(max_val), label = "Y-axis range", 
              value = c(floor(min_val), ceiling(max_val)), step = 1)
  
})

output$scatter_plot_render <- renderUI({
  plotOutput("scatterPlot", height = paste0(input$plot_size_scatter), click = "scatter_click")
})

plotDat <- reactive({
  dat <- geochemDat()
  
  locations <- input$select_locations_scatter
  samples <- input$exclude_samples_scatter
  sources <- input$exclude_sources_scatter
  
  plotDat <- dat[,1:4]
  if(input$select_b_scatter == "-"){
    plotDat$x <- as.numeric(dat[,input$select_a_scatter])
  }else{
    if(input$scatter_radio_x == "/"){
    plotDat$x <- as.numeric(dat[,input$select_a_scatter])/as.numeric(dat[,input$select_b_scatter])
    }else{
      plotDat$x <- as.numeric(dat[,input$select_a_scatter]) + as.numeric(dat[,input$select_b_scatter])
    }  
    }
  if(input$select_d_scatter == "-"){
    plotDat$y <- as.numeric(dat[,input$select_c_scatter])
  }else{
    if(input$scatter_radio_y == "/"){
      plotDat$y <- as.numeric(dat[,input$select_c_scatter])/as.numeric(dat[,input$select_d_scatter])
    }else{
      plotDat$y <- as.numeric(dat[,input$select_c_scatter]) + as.numeric(dat[,input$select_d_scatter])
    }
    }
  
  if(!is.null(locations)){
    if(locations[1] != "-"){
    selected_locations <- plotDat$Locality %in% locations
    plotDat <- plotDat[selected_locations,]
    }
  }
  
  if(!is.null(samples)){
    if(samples[1] != "-"){
      selected_samples <- plotDat$Sample.ID %in% samples
      plotDat <- plotDat[!selected_samples,]
    }
  }
  
  if(!is.null(sources)){
    if(sources[1] != "-"){
      selected_sources <- plotDat$Source %in% sources
      plotDat <- plotDat[!selected_sources,]
    }
  }

  plotDat
})

scatter_trend <- reactive({
  plotDat <- plotDat()
  fit <- lm(y ~ x, data = plotDat, na.action = 'na.exclude')
  fit
})

output$scatter_trend_output <- renderText({
 fit <-scatter_trend()
 fitSum <- summary(fit)
 trend_val <- paste0("Intercept: ", round(as.numeric(fit$coefficients[1]), 2), ", Slope: ", 
                     round(as.numeric(fit$coefficients[2]), 2), ", R-Squared: ", round(fitSum$r.squared, 2))
 trend_val
})

output$scatterPlot <- renderPlot({
  if(input$select_b_scatter == "-"){
    xlabel <- input$select_a_scatter
  }else{
    if(input$scatter_radio_x == "/"){
    xlabel <- paste0(input$select_a_scatter, "/", input$select_b_scatter)
    }else{
      xlabel <- paste0(input$select_a_scatter, "+", input$select_b_scatter)
      
    }
  }
  
  if(input$select_d_scatter == "-"){
    ylabel <- input$select_c_scatter
  }else{
    if(input$scatter_radio_y == "/"){
    ylabel <- paste0(input$select_c_scatter, "/", input$select_c_scatter)
    }else{
      ylabel <- paste0(input$select_c_scatter, "+", input$select_c_scatter)
    }
  }
  
  shapes <- data.frame(Source = c('Suda et al. (1982)', 'Vazquez (1998)', 'Alibert et al. (1986)', 
                                  'Nathalie PhD', 'Hooten (1999)', 'Nathalie 2015', '2012 Workshop', 
                                  'Pepe PhD'),
                       shape = c(3, 4, 14, 15, 16, 18, 19, 8)
  )
  
  plotDat <- plotDat()
  plotDat <- plotDat %>% left_join(shapes, by = "Source") %>% filter(Source != "")
  fit <- scatter_trend()
  int_val <- fit$coefficients[1]
  slope_val <- fit$coefficients[2]
  if(input$scatter_show_trend){
  p <- ggplot() +
    geom_point(data = plotDat, aes (x = x, y = y, color = Locality,  fill = Locality), 
                                      shape = plotDat$shape, size = input$scatter_size, show.legend = input$scatter_show_legend) +
    geom_abline(intercept = int_val, slope = slope_val)
  }else{
    p <- ggplot() +
      geom_point(data = plotDat, aes (x = x, y = y, color = Locality,  fill = Locality), 
                 shape = plotDat$shape, size = input$scatter_size, show.legend = input$scatter_show_legend)
  }
  
  p + 
    xlab(xlabel) +
    ylab(ylabel) +
    xlim(as.numeric(input$scatter_x)) + 
    ylim(as.numeric(input$scatter_y)) + 
    theme_minimal(base_size = 14)
})

output$scatter_value <- renderText({
  input$scatter_click
  selected_values <- '-'
  plotDat <- plotDat()
  res <- nearPoints(plotDat, input$scatter_click, allRows = T)
  selected_samples <- paste(unique(res[res$selected_, 1]), collapse = ", ")
  selected_locations <- paste(unique(res[res$selected_, 2]), collapse = ", ")
  selected_values <- paste0('Samples: ', selected_samples, ". Localities: ", selected_locations, ".")
  selected_values
})

output$scatter_table <- renderDataTable({
  input$scatter_click
  selected_values <- '-'
  plotDat <- plotDat()
  res <- nearPoints(plotDat, input$scatter_click, allRows = T)
  if(length(res$selected_[res$selected_]) < 1) {
    DT::datatable(plotDat, options = list(pageLength = 25))
  }else{
  DT::datatable(res[res$selected_,1:(ncol(res)- 1)], options = list(pageLength = 25))
  }
})
  
# Spider ------------------------------------------------------------------

output$select_locations_spider_output <- renderUI({
  selectInput("select_locations_spider", label = 'Select Locations:', 
              choices = c("-", unique(geochemDat()$Locality)), multiple = T, 
              selected = '-')
})
output$exclude_samples_spider_output <- renderUI({
  selectInput("exclude_samples_spider", label = 'Exclude Samples:', 
              choices = c("-", unique(geochemDat()$Sample.ID)), multiple = T, 
              selected = '-')
})
output$exclude_sources_spider_output <- renderUI({
  selectInput("exclude_sources_spider", label = 'Exclude Sources:', 
              choices = c("-", unique(geochemDat()$Source)), multiple = T, 
              selected = '-')
})  

output$spider_plot_render <- renderUI({
  plotOutput("spiderPlot", height = paste0(input$plot_size_spider), click = "spider_click", hover = "spider_hover")
  
})

  spiderLong <- reactive({
    dat <- geochemDat()
    
    locations <- input$select_locations_spider
    samples <- input$exclude_samples_spider
    sources <- input$exclude_sources_spider
    colnames(dat) <- gsub('.Spider', '', colnames(dat))
    spider <- dat[,92:107]  
    tmp <- dat[,1:4]
    spider <- spider %>% mutate_all(as.numeric)
    spider <- tmp %>% bind_cols(spider)
    spiderLong <- spider %>% gather(x, value, Th:Lu, factor_key = T) %>% 
      mutate(log_value = log10(value))
    if(!is.null(locations)){
      if(locations[1] != "-"){
        selected_locations <- spiderLong$Locality %in% locations
        spiderLong <- spiderLong[selected_locations,]
      }
    }
    
    if(!is.null(samples)){
      if(samples[1] != "-"){
        selected_samples <- spiderLong$Sample.ID %in% samples
        spiderLong <- spiderLong[!selected_samples,]
      }
    }
    
    if(!is.null(sources)){
      if(sources[1] != "-"){
        selected_sources <- spiderLong$Source %in% sources
        spiderLong <- spiderLong[!selected_sources,]
      }
    }
    
    
    spiderLong
  })
  
  output$spiderPlot <- renderPlot({
    spiderLong <- spiderLong()
    ggplot(data = spiderLong) +
      geom_point(aes(x = x, y = value, group = Sample.ID, color = Locality)) +
      geom_line(aes(x = x, y = value, group = Sample.ID, color = Locality)) +
      scale_y_continuous(trans = 'log10')  + 
      xlab("Element") +
      ylab("Weight pct (shown with log scale)") +
      theme_minimal(base_size = 14)
  })
   
  output$spider_value <- renderText({
    input$spider_click
    selected_values <- '-'
    spiderLong <- spiderLong()
    res <- nearPoints(spiderLong, input$spider_click, allRows = T)
    selected_samples <- paste(unique(res[res$selected_, 1]), collapse = ", ")
    selected_locations <- paste(unique(res[res$selected_, 2]), collapse = ", ")
    selected_values <- paste0('Samples: ', selected_samples, ". Localities: ", selected_locations, ".")
    selected_values
  })
  
  output$spider_table <- renderDataTable({
    input$spider_click
    selected_values <- '-'
    spiderLong <- spiderLong()
    res <- nearPoints(spiderLong, input$spider_click, allRows = T)
    selected_samples <- unique(res[res$selected_, 1])
    
    selected_rows <- match(selected_samples, geochemDat()$Sample.ID)
    if(length(selected_rows) > 0){  
      DT::datatable(geochemDat()[selected_rows,c(1:4, 92:107)], options = list(pageLength = 25))
    }else{
      DT::datatable(geochemDat()[,c(1:4, 92:107)], options = list(pageLength = 25))
    }
    
  })
  

# Chond -------------------------------------------------------------------

  #TODO change to REE Plot
  
  output$select_locations_chond_output <- renderUI({
    selectInput("select_locations_chond", label = 'Select Locations:', 
                choices = c("-", unique(geochemDat()$Locality)), multiple = T, 
                selected = '-')
  })
  output$exclude_samples_chond_output <- renderUI({
    selectInput("exclude_samples_chond", label = 'Exclude Samples:', 
                choices = c("-", unique(geochemDat()$Sample.ID)), multiple = T, 
                selected = '-')
  })
  output$exclude_sources_chond_output <- renderUI({
    selectInput("exclude_sources_chond", label = 'Exclude Sources:', 
                choices = c("-", unique(geochemDat()$Source)), multiple = T, 
                selected = '-')
  })  

  output$chond_plot_render <- renderUI({
    plotOutput("chondPlot", height = paste0(input$plot_size_chond), click = "chond_click", hover = "chond_hover")

  })
  
    
  chondLong <- reactive({
    ##remove zeros?
    
    
    dat <- geochemDat()
    
    locations <- input$select_locations_chond
    samples <- input$exclude_samples_chond
    sources <- input$exclude_sources_chond
    colnames(dat) <- gsub('.chond', '', colnames(dat))
    
    chond <- dat[,78:91]  
    tmp <- dat[,1:4]
    chond <- chond %>% mutate_all(as.numeric)
    chond <- tmp %>% bind_cols(chond)
    chondLong <- chond %>% gather(x, value, La:Lu, factor_key = T) %>% 
      mutate(log_value = log10(value))
    if(!is.null(locations)){
      if(locations[1] != "-"){
        selected_locations <- chondLong$Locality %in% locations
        chondLong <- chondLong[selected_locations,]
      }
    }
    
    if(!is.null(samples)){
      if(samples[1] != "-"){
        selected_samples <- chondLong$Sample.ID %in% samples
        chondLong <- chondLong[!selected_samples,]
      }
    }
    
    if(!is.null(sources)){
      if(sources[1] != "-"){
        selected_sources <- chondLong$Source %in% sources
        chondLong <- chondLong[!selected_sources,]
      }
    }
    if(input$chond_remove_zero){
      chondLong[chondLong == 0] <- NA
    }
    chondLong
  })
  
  output$chondPlot <- renderPlot({
    chondLong <- chondLong()
    ggplot(data = chondLong) +
      geom_point(aes(x = x, y = value, group = Sample.ID, color = Locality)) +
      geom_line(aes(x = x, y = value, group = Sample.ID, color = Locality)) +
      scale_y_continuous(trans = 'log10')  + 
      xlab("Element") +
      ylab("Weight pct normalised to Chond (shown with log scale)") +
      theme_minimal(base_size = 14)
  })
  
  output$chond_value <- renderText({
    input$chond_click
    selected_values <- '-'
    chondLong <- chondLong()
    res <- nearPoints(chondLong, input$chond_click, allRows = T)
    selected_samples <- paste(unique(res[res$selected_, 1]), collapse = ", ")
    selected_locations <- paste(unique(res[res$selected_, 2]), collapse = ", ")
    selected_values <- paste0('Samples: ', selected_samples, ". Localities: ", selected_locations, ".")
    selected_values
  })
  
  output$chond_table <- renderDataTable({
    input$chond_click
    selected_values <- '-'
    chondLong <- chondLong()
    res <- nearPoints(chondLong, input$chond_click, allRows = T)
    selected_samples <- unique(res[res$selected_, 1])
    
    selected_rows <- match(selected_samples, geochemDat()$Sample.ID)
    if(length(selected_rows) > 0){  
      DT::datatable(geochemDat()[selected_rows,c(1:4, 78:91)], options = list(pageLength = 25))
    }else{
      DT::datatable(geochemDat()[,c(1:4, 78:91)], options = list(pageLength = 25))
    }
    
  })
  
  

   
# One vs multiple ---------------------------------------------------------
  
  output$select_a_multiplot_output <- renderUI({
    selectInput("select_a_multiplot", label = "Select column for 'A':", choices = colnames(geochemDat()[5:ncol(geochemDat())]), multiple = F)
  }) 
  output$select_b_multiplot_output <- renderUI({
    selectInput("select_b_multiplot", label = "Select column for 'B':", choices = c("-", colnames(geochemDat()[5:ncol(geochemDat())])), selected = "-", multiple = F)
  })  
  output$select_c_multiplot_output <- renderUI({
    selectInput("select_c_multiplot", label = "Select column for 'C':", choices = colnames(geochemDat()[5:ncol(geochemDat())]), selected = "SiO2", multiple = T)
  })  
  
  output$select_locations_multiplot_output <- renderUI({
    selectInput("select_locations_multiplot", label = 'Select Locations:', 
                choices = c("-", unique(geochemDat()$Locality)), multiple = T, 
                selected = '-')
  })
  output$exclude_samples_multiplot_output <- renderUI({
    selectInput("exclude_samples_multiplot", label = 'Exclude Samples:', 
                choices = c("-", unique(geochemDat()$Sample.ID)), multiple = T, 
                selected = '-')
  })
  output$exclude_sources_multiplot_output <- renderUI({
    selectInput("exclude_sources_multiplot", label = 'Exclude Sources:', 
                choices = c("-", unique(geochemDat()$Source)), multiple = T, 
                selected = '-')
  })
  
  output$multiplot_render <- renderUI({
    plotOutput("multiPlot", height = paste0(input$plot_size, "px"), click = "multiplot_click", hover = "multiplot_hover")
  })
  
  multiPlotDat <- reactive({
    dat <- geochemDat()
    
    locations <- input$select_locations_multiplot
    samples <- input$exclude_samples_multiplot
    sources <- input$exclude_sources_multiplot
    
    multiPlotDat <- dat[,1:4]
    if(input$select_b_multiplot == "-"){
      multiPlotDat$x <- as.numeric(dat[,input$select_a_multiplot])
    }else{
      multiPlotDat$x <- as.numeric(dat[,input$select_a_multiplot])/as.numeric(dat[,input$select_b_multiplot])
    }
    #TODO fix this to allow for multiple plots
      multiPlotDat[,6:(5 + length(input$select_c_multiplot))] <- dat[,input$select_c_multiplot]
     multiPlotDat[,6:(5 + length(input$select_c_multiplot))] <- sapply(multiPlotDat[,6:(5 + length(input$select_c_multiplot))], as.numeric) 
    if(!is.null(locations)){
      if(locations[1] != "-"){
        selected_locations <- multiPlotDat$Locality %in% locations
        multiPlotDat <- multiPlotDat[selected_locations,]
      }
    }
    
    if(!is.null(samples)){
      if(samples[1] != "-"){
        selected_samples <- multiPlotDat$Sample.ID %in% samples
        multiPlotDat <- multiPlotDat[!selected_samples,]
      }
    }
    
    if(!is.null(sources)){
      if(sources[1] != "-"){
        selected_sources <- multiPlotDat$Source %in% sources
        multiPlotDat <- multiPlotDat[!selected_sources,]
      }
    }
    
    multiPlotDat
  })
  
  output$multiPlot <- renderPlot({
    
    plts <- list()
    for(i in 1:length(input$select_c_multiplot)){
      
      pDat <- multiPlotDat()[,1:5]
      pDat$y <- multiPlotDat()[,(5+i)]
      
      if(input$select_b_multiplot == "-"){
        xlabel <- input$select_a_multiplot
      }else{
        xlabel <- paste0(input$select_a_multiplot, "/", input$select_b_multiplot)
      }
      
        ylabel <- input$select_c_multiplot[i]
     
      
      
      
      plts[[i]] <- ggplot(data = pDat, 
                          aes (x = x, y = y, 
                               color = Locality,  fill = Locality)) +
        geom_point(show.legend = F)  + 
        xlab(xlabel) +
        ylab(ylabel) +
        theme_minimal(base_size = 14)
    }
    
    pltCols <- 3
    if(length(input$select_c_multiplot) <=3){
      pltCols <- length(input$select_c_multiplot)
    }
    
    pltRows <- ceiling(length(input$select_c_multiplot) / 3)
    
    ggarrange(plotlist = plts, ncol = pltCols, nrow = pltRows)
    
  })
  
  output$multiplot_value <- renderText({
    input$multiplot_click
    selected_values <- '-'
    plotDat <- multiPlotDat()
    res <- nearPoints(plotDat, input$multiplot_click, allRows = T)
    selected_samples <- paste(unique(res[res$selected_, 1]), collapse = ", ")
    selected_locations <- paste(unique(res[res$selected_, 2]), collapse = ", ")
    selected_values <- paste0('Samples: ', selected_samples, ". Localities: ", selected_locations, ".")
    selected_values
  })
  
  output$multiplot_table <- renderDataTable({
    DT::datatable(multiPlotDat(), options = list(pageLength = 25))
  })
  
# All data ----------------------------------------------------------------
  output$select_locations_data_output <- renderUI({
    selectInput("select_locations_data", label = 'Select Locations:', 
                choices = c("-", unique(geochemDat()$Locality)), multiple = T, 
                selected = '-')
  })
  output$exclude_samples_data_output <- renderUI({
    selectInput("exclude_samples_data", label = 'Exclude Samples:', 
                choices = c("-", unique(geochemDat()$Sample.ID)), multiple = T, 
                selected = '-')
  })
  output$exclude_sources_data_output <- renderUI({
    selectInput("exclude_sources_data", label = 'Exclude Sources:', 
                choices = c("-", unique(geochemDat()$Source)), multiple = T, 
                selected = '-')
  })
  output$all_data_table <- renderDataTable({
    geochemDat <- read.csv('geochem.csv')
    
    DT::datatable(geochemDat, options = list(pageLength = 25))
  })
  
  }



# Run the application 
shinyApp(ui = ui, server = server)



