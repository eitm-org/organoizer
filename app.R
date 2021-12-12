library(shiny)
VERSION <- numeric_version("0.2.11")

#Setting up
cwd <- getwd()
source(file = file.path(cwd, "lib", "organoid_funcs.R"),
       local = TRUE)
options(shiny.maxRequestSize = 30 * 1024 ^ 2) #Allows for large files to be uploaded

#Not to be included in the list of features to plot
invalid_features <- c(
  "Compound",
  "Concentration",
  "Cell Type",
  "Cell Count",
  "Day",
  "LD",
  "Display",
  "Row",
  "Column",
  "Plane",
  "Timepoint",
  "Time [s]",
  "Field",
  "Object ID",
  "X",
  "Y"
)


####UI####
##############
ui <- fluidPage(
  titlePanel(
    paste(
      "Organoizer (v. ",
      VERSION,
      ") - Extract and plot experimental results for organoid data"
    )
  ),

  sidebarLayout(
    sidebarPanel(
      fileInput(
        'input_file',
        'Upload experiment data in multi-sheets Excel format',
        accept = c(
          'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
        )
      ),
      
      uiOutput("pat_id"),
      uiOutput("feature"),
      uiOutput("missing_as_zero"),
      uiOutput("metric"),
      uiOutput("plot_options"),
      uiOutput("norm_to_first"),
      uiOutput("error_bars"),
      uiOutput("freescale"),
      uiOutput("show_boxplot"),
      uiOutput("downloadDataB"),
      uiOutput("downloadLinePB"),
      uiOutput("downloadBoxPB")
    ),
    
    mainPanel(
      plotOutput("lineplot"),
      conditionalPanel(condition = "input.show_boxplot==true", plotOutput("boxplot"))
    )
  )
)

####SERVER####
##############
server <- function(input, output) {
  #Data handling functions
  #Load the worksheet
  filedata <- reactive({
    infile <- input$input_file
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    new_filepath <-
      paste0(infile$datapath, '.xlsx')  #readxl requires the xlsx extension for the input file
    file.rename(infile$datapath, new_filepath)
    infile$datapath <- new_filepath
    import_wb(infile$datapath)
  })
  
  #Normalized data
  normdata <- reactive({
    df <- subdata()
    if (is.null(df))
      return(NULL)
    normalize_data(df,
                   metric = input$metric)
  })
  
  #Subset data for a specific feature
  subdata <- reactive({
    df <- filedata()
    if (is.null(df) || is.null(input$feature))
      return(NULL)
    subset_measurements(
      df,
      feature = input$feature,
      missing_as_zero = input$missing_as_zero
    )
  })
  
  #Prepares the data for download
  downloadDataFormat <- reactive({
    outdata <- normdata()
    if (is.null(outdata)) {
      return(NULL)
    }
    dl <- format_for_dl(
      outdata,
      patient = input$pat_id,
      feature = input$feature,
      metric = input$metric
    )
  })
  
  #Plotting functions
  plotLines <- reactive({
    plot_curves(
      normdata(),
      feature = input$feature,
      patient = input$pat_id,
      error_bars = input$error_bars,
      normalized = input$norm_to_first,
      metric = input$metric,
      freescale = input$freescale
    )
  })
  
  plotBoxes <- reactive({
    plot_boxes(subdata(),
               feature = input$feature,
               patient = input$pat_id)
  })
  
  #Plots
  output$lineplot <- renderPlot({
    if (is.null(normdata()) || is.null(input$error_bars))
      return(NULL)
    plot(plotLines())
  })
  
  output$boxplot <- renderPlot({
    if (is.null(subdata()))
      return(NULL)
    plot(plotBoxes())
  })
  
  #Handlers
  output$downloadDataH <- downloadHandler(
    filename = function() {
      paste0(downloadDataFormat()[[1]], '.csv')
    },
    content = function(con) {
      write.csv(downloadDataFormat()[[2]], con)
    }
  )
  
  output$downloadLinePH <- downloadHandler(
    filename = function() {
      paste0(downloadDataFormat()[[1]], '.png')
    },
    content = function(con) {
      ggsave(con, plot = plotLines(), device = "png")
    }
  )
  
  output$downloadBoxPH <- downloadHandler(
    filename = function() {
      paste0(downloadDataFormat()[[1]], '_boxplot.png')
    },
    content = function(con) {
      sd <- subdata()
      if (is.null(sd))
        return(NULL)
      nplots <- length(unique(sd[, "Cell Type"]))
      if (nplots == 1) {
        ggsave(
          con,
          plot = plotBoxes(),
          device = "png",
          height = 3.5,
          units = 'in'
        )
      }
      else{
        ggsave(con, plot = plotBoxes(), device = "png")
      }
    }
  )
  
  #UI elements
  output$pat_id <- renderUI({
    df <- filedata()
    if (is.null(df))
      return(NULL)
    temp_name <- gsub('.xlsx', '', input$input_file$name)
    textInput('pat_id', "Enter the patient identifier", value = temp_name)
  })
  
  output$feature <- renderUI({
    df <- filedata()
    if (is.null(df))
      return(NULL)
    items = c('counts', subset(names(df),!(names(df) %in% invalid_features)))
    names(items) = items
    selectInput('feature', h4("Feature to plot"), items)
  })
  
  output$missing_as_zero <- renderUI({
    df <- filedata()
    if (is.null(df))
      return(NULL)
    checkboxInput('missing_as_zero', "Treat missing data as 0", TRUE)
  })
  
  output$metric <- renderUI({
    df <- filedata()
    if (is.null(df))
      return(NULL)
    radioButtons(
      'metric',
      h4("Metric to use"),
      c(
        "Proportion of Live over total" = "proportion",
        "Ratio Live/Dead" = "ratio"
      )
    )
  })
  
  output$plot_options <- renderUI({
    if (is.null(filedata()))
      return(NULL)
    h4("Plot options")
  })
  
  output$norm_to_first <- renderUI({
    if (is.null(filedata()))
      return(NULL)
    checkboxInput('norm_to_first', "Plot values normalized to first day", TRUE)
  })
  
  output$error_bars <- renderUI({
    if (is.null(filedata()))
      return(NULL)
    checkboxInput('error_bars', "Plot standard deviation", FALSE)
  })
  
  output$freescale <- renderUI({
    if (is.null(filedata()))
      return(NULL)
    checkboxInput('freescale', "Independent y-axes", FALSE)
  })
  
  output$show_boxplot <- renderUI({
    if (is.null(filedata()))
      return(NULL)
    checkboxInput('show_boxplot', "Display the boxplot (slow)", FALSE)
  })
  
  output$downloadDataB <- renderUI({
    if (is.null(normdata())) {
      return(NULL)
    }
    downloadButton('downloadDataH', 'Download data')
  })
  
  output$downloadLinePB <- renderUI({
    if (is.null(normdata())) {
      return(NULL)
    }
    downloadButton('downloadLinePH', 'Download line plot')
  })
  
  output$downloadBoxPB <- renderUI({
    if (is.null(subdata()) ||
        is.null(input$show_boxplot) || !(input$show_boxplot)) {
      return(NULL)
    }
    downloadButton('downloadBoxPH', 'Download box plot')
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)
