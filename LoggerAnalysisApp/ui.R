library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Analyize your Logger Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput("dat", "Upload Logger Data", multiple = FALSE, accept = NULL, width = NULL),
      numericInput("center_freq", "Center Frequency kHz", value = 150175),
      numericInput("lines_to_skip", "Lines in File to skip", value = 0),
      tags$hr(),
      h4("Filter of Signal Length"),
      checkboxInput("filter_length","Activate",value = FALSE),
      sliderInput("signal_length",
                  "Length:",
                  min = 0.0005,
                  max = 0.050,
                  value = c(0.005,0.03)),
      tags$hr(),
      h4("Multiple Frequency Filter"),
      checkboxInput("filter_freq","Activate",value = FALSE),
      h5("Settings see tab"),
      tags$hr(),
      h4("Single Frequency Filter"),
      checkboxInput("filter_one_freq","Activate",value = FALSE),
      numericInput("single_freq", "Single Frequency kHz", value = 150175),
      sliderInput("freq_error",
                  "Frequency Error:",
                  min = 1,
                  max = 30,
                  value = 5),
      textOutput("total_counts")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Frequency",
                           plotOutput("histo")
                           ),
                  tabPanel("Duration",
                           plotOutput("histo_length")
                  ),
                  tabPanel("Signal Strength",
                           plotOutput("histo_strength")
                  ),
                  tabPanel("Results",plotOutput("facet")),
                  tabPanel("Frequencies",
                           downloadButton('downloadData', 'Download'),
                           fileInput("freq_file", "Upload Logger Data", multiple = FALSE, accept = NULL, width = NULL)
                  ),
                  tabPanel("Manual Frequencies",
                           column(3,
                                  numericInput("freq1", "Frequency 1", value = 150020),
                                  textInput("freq_id_1", "Label 1", value = "Label 1"),
                                  checkboxInput("act_freq_1", "Activate 1", value = FALSE),
                                  tags$hr(),
                                  numericInput("freq4", "Frequency 4", value = 150080),
                                  textInput("freq_id_4", "Label 4", value = "Label 4"),
                                  checkboxInput("act_freq_4", "Activate 4"),
                                  tags$hr(),
                                  numericInput("freq7", "Frequency 7", value = 150140),
                                  textInput("freq_id_7", "Label 7", value = "Label 7"),
                                  checkboxInput("act_freq_7", "Activate 7")
                           ),
                           column(3,
                                  numericInput("freq2", "Frequency 2", value = 150040),
                                  textInput("freq_id_2", "Label 2", value = "Label 2"),
                                  checkboxInput("act_freq_2", "Activate 2"),
                                  tags$hr(),
                                  numericInput("freq5", "Frequency 5", value = 150100),
                                  textInput("freq_id_5", "Label 5", value = "Label 5"),
                                  checkboxInput("act_freq_5", "Activate 5"),
                                  tags$hr(),
                                  numericInput("freq8", "Frequency 8", value = 150160),
                                  textInput("freq_id_8", "Label 8", value = "Label 8"),
                                  checkboxInput("act_freq_8", "Activate 8")
                                  ),
                           column(3,
                                  numericInput("freq3", "Frequency 3", value = 150060),
                                  textInput("freq_id_3", "Label 3", value = "Label 3"),
                                  checkboxInput("act_freq_3", "Activate 3"),
                                  tags$hr(),
                                  numericInput("freq6", "Frequency 6", value = 150120),
                                  textInput("freq_id_6", "Label 6", value = "Label 6"),
                                  checkboxInput("act_freq_6", "Activate 6"),
                                  tags$hr(),
                                  numericInput("freq9", "Frequency 9", value = 150180),
                                  textInput("freq_id_9", "Label 9", value = "Label 9"),
                                  checkboxInput("act_freq_9", "Activate 9")
                           )
                        )
      )
    )
  )
))
