library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(plotly)
library(shinydashboard)
library(shinyWidgets)


# Load the dataset for the app
data <- read.csv("licensed_drivers.csv")
states <- sort(unique(data$State))

# Define UI for application that plots features of movies -----------
ui <- dashboardPage(
  
  # Application title -----------------------------------------------
  dashboardHeader(title = "Licensed Drivers Dashboard"),
  
  # Sidebar layout with a input and output definitions --------------
  dashboardSidebar(
    
    tags$style(HTML(".sidebar { height: calc(100vh - 50px) !important; overflow-y: auto; }")
  ),
    
    # Inputs: Select variables to plot ------------------------------
    selectInput(inputId = "selected_state", 
                label = "Select State:",
                choices = sort(unique(data$State)),
                selected = "pennsylvania"),
    
    # Select which gender group to display
    radioButtons(inputId = "selected_gender",
                 label = "Select Gender:",
                 choices = c("Female", "Male")),
    
    # Select the Year range
    sliderInput(inputId = "selected_year", 
                label = "Select Year Range", 
                min = 1994, max = 2018, 
                value = c(2000, 2010)),
    
    # Select which age group to display
    checkboxGroupInput(inputId = "selected_cohort",
                       label = "Select Age group:",
                       choices = sort(unique(data$Cohort))),
    
    # Show data table 
    checkboxInput(inputId = "show_data",
                  label = "Show data table",
                  value = TRUE),
    
    # Add a download button
    downloadButton("downloadData", "Download Data")
  ),
  
  # Output
  dashboardBody(
    # Value Boxes ----------------------------------------------
    fluidRow(
      valueBoxOutput("total_drivers", width = 4),
      valueBoxOutput("AAGR", width = 4),
      valueBoxOutput("senior_drivers", width = 4),
    ),
    
    tabBox(
      title = "Plots",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "250px",
      tabPanel("bar Plot", plotlyOutput("barPlot", height = "400px")),
      tabPanel("Line Graph", plotlyOutput("linePlot", height = "400px")),
      tabPanel("Heat Map", plotlyOutput("heatMap", height = "400px"))),
    

    # Add the data table
    fluidRow(
      box(
        title = "Licensed Drivers Data",
        status = "primary",
        solidHeader = TRUE,
        DT::dataTableOutput(outputId = "driversTable")
      )
    )
  )
)

# Define server function required to create the plots
server <- function(input, output) {
  # Filter the data based on the inputs
  drivers_subset <- reactive({ 
    req(input$selected_gender)
    filter(data, Gender %in% input$selected_gender, State %in% input$selected_state,
           Cohort %in% input$selected_cohort, Year >= input$selected_year[1],
           Year <= input$selected_year[2])
  })
  
  
  # Print data table if checked
  output$driversTable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = drivers_subset(), 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
  
  output$linePlot <- renderPlotly({
    if (nrow(drivers_subset()) == 0) {
      return(NULL)
    }
    ggplot(data = drivers_subset(),aes(x=Year, y=drivers_sum, group=Gender, color=Cohort)) + 
      geom_line(aes(group = Cohort)) + 
      labs(title="Line Graph") + 
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = "Year", y = "Drivers") + 
      geom_point() + 
      scale_y_continuous(labels = scales::comma) + 
      scale_x_continuous(breaks = seq(1994, 2018, 1))
  })
  
  output$barPlot <- renderPlotly({
    if (nrow(drivers_subset()) == 0) {
      return(NULL)
    }
    ggplot(data = drivers_subset(), aes(x = Year, y = drivers_sum, fill = Cohort)) +
      geom_bar(color = "black",position="stack", stat="identity") + 
      labs(title="Bar Plot") + 
      scale_fill_brewer(palette = "Set3") +
      theme(plot.title = element_text(hjust = 0.5)) + 
      labs(x = "Year", y = "Drivers") + 
      scale_y_continuous(labels = scales::comma) + 
      scale_x_continuous(breaks = seq(1994, 2018, 1))
  })
  
  output$heatMap <- renderPlotly({
    if (nrow(drivers_subset()) == 0) {
      return(NULL)
    }
      # Create heatmap plot
      ggplot(data = drivers_subset(), aes(x = Year, y = Cohort, fill = drivers_sum)) +
        geom_tile(color = "black") +
        scale_fill_gradient(low = "white", high = "red") +
        scale_x_continuous(breaks = seq(min(drivers_subset()$Year), max(drivers_subset()$Year), 1),
                           labels = as.character(seq(min(drivers_subset()$Year), max(drivers_subset()$Year), 1)))
  })
  
  # Download the filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data_", input$selected_state, "_", input$selected_gender, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(drivers_subset(), file, row.names = FALSE)
    }
  )
  
  output$total_drivers <- renderValueBox({
    valueBox(
      paste0(format(sum(drivers_subset()$drivers_sum), big.mark = ",")), 
      "Total Drivers", 
      icon = icon("car"), 
      color = "blue"
    )
  })
  
  output$AAGR <- renderValueBox({
    # get the selected year from an input control called `selected_year`
    selected_year <- input$selected_year
    
    # calculate the AAGR for the selected year
    data_summed <- drivers_subset() %>% group_by(Year) %>% summarise(drivers_sum = sum(drivers_sum))
    aagr <- (data_summed$drivers_sum[length(data_summed$drivers_sum)] / data_summed$drivers_sum[1])^(1/length(data_summed$drivers_sum)) - 1

    # initialize as 0
    if (is.null(input$selected_cohort)) {
      aagr <- 0
    }
    formatted_aagr <- sprintf("%.2f%%", aagr*100)
    valueBox(
      paste0(sprintf("%.2f%%", aagr*100)),
      "AAGR for Selected Year",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  # Total number of senior drivers
  output$senior_drivers <- renderValueBox({
    senior_cohort <- c("60-64", "65-69", "70-74", "75-79","80-84","85+")
    valueBox(
      paste0(format(sum(filter(drivers_subset(), Cohort %in% senior_cohort)$drivers_sum), big.mark = ",")), 
      "Senior Drivers(>60 years old)", 
      color = "maroon"
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
