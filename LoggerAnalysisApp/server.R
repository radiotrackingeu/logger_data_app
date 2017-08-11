library(shiny)
require(ggplot2)
options(shiny.maxRequestSize=300*1024^2)

#Some functions first


print(getwd())
source("signal_analysis.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  tmp<- reactive({
    inFile <- input$dat
    
    if (is.null(inFile))
      return(NULL)
    return(read_logger_data(inFile$datapath,input$lines_to_skip,input$center_freq))
  })
  
  freqs <- reactive({
    inFile <- input$freq_file
    if(is.null(inFile)){
      tmp<-NULL
      for(i in 1:9)
        if(input[[paste0("act_freq_",i)]]){
          zws<-data.frame(freq=input[[paste0("freq",i)]],label=input[[paste0("freq_id_",i)]])
          tmp<-rbind.data.frame(tmp,zws)
        }
      return(tmp)
    }
    else{
      return(read.csv2(inFile$datapath))
    }

    

    
  })

  data <- reactive({
    if (is.null(tmp()))
      return(NULL)
    tempo<-tmp()
    if(input$filter_length){
      tempo<-filter_data_length(tempo,input$signal_length)
    }
    if(input$filter_one_freq){
      tempo<-filter_data_freq(tempo,input$single_freq,input$freq_error,input$center_freq)
    }
    if(input$filter_freq){
      tempo<-filter_data_freq(tempo,freqs()[["freq"]],input$freq_error,input$center_freq,freqs()[["label"]])
    }
    if(input$filter_freq&&input$filter_one_freq){
      return(NULL)
    }
    return(tempo)
  } )
  
  output$facet <- renderPlot({
    
    if(is.null(data()))
      return(NULL)
    if(input$filter_freq){
      plot_time_signal(data(),input$center_freq,TRUE)
    }
    else{
      plot_time_signal(data(),input$center_freq,input$filter_one_freq)
    }
    
    })
  
  output$histo <- renderPlot({
    if(is.null(data()))
      return(NULL)
    
    hist_signalplot_time_signal(data())
  })
  
  output$histo_length <- renderPlot({
    require("ggplot2")
    if(is.null(tmp())){
      return(NULL)
    }
      
    ggplot(data()) + geom_histogram(aes(Duration),bins= 100)
  })
  
  output$histo_strength <- renderPlot({
    if (is.null(data()))
      return(NULL)
    
    ggplot(data()) + geom_histogram(aes(HighLevel),bins= 200)
  })
  output$total_counts<-renderText({
    if (is.null(tmp()))
      return(NULL)
    
    return(paste("Number of observations in plot",dim(data())[1],"of total", dim(tmp())[1]))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("frequencies-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv2(freqs(), file)
    }
  )
  
})
