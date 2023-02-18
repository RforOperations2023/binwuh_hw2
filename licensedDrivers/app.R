library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(shinydashboard)

# Load the dataset for the app
data <- read.csv("licensed_drivers.csv")
states <- sort(unique(data$State))

# Define UI for application that plots features of movies -----------
ui <- dashboardPage(
  
  # Application title -----------------------------------------------
  dashboardHeader(title = "Licensed Drivers Dashboard"),
  
  # Sidebar layout with a input and output definitions --------------
  dashboardSidebar(
    
    # Inputs: Select variables to plot ------------------------------
    selectInput(inputId = "selected_state", 
                label = "Select State:",
                choices = sort(unique(data$State)),
                selected = "pennsylvania"),
    
    # Select which gender group to display
    radioButtons(inputId = "selected_gender",
                 label = "Select Gender:",
                 choices = c("Female", "Male")),
    
    # Select which age group to display
    checkboxGroupInput(inputId = "selected_cohort",
                       label = "Select Age group:",
                       choices = sort(unique(data$Cohort))),
    
    # Show data table 
    checkboxInput(inputId = "show_data",
                  label = "Show data table",
                  value = TRUE),
    
    # Select the Year range
    sliderInput(inputId = "selected_year", 
                label = "Select Year Range", 
                min = 1994, max = 2018, 
                value = c(2000, 2010)),
    
    # Horizontal line for visual separation 
    tags$hr(),
    
    # Add a download button
    downloadButton("downloadData", "Download Data")
  ),
  
  # Output
  dashboardBody(
    # Value Boxes ----------------------------------------------
    fluidRow(
      valueBoxOutput("total_drivers", width = 4),
      valueBoxOutput("female_drivers", width = 4),
      valueBoxOutput("male_drivers", width = 4)
    ),
    
    br(),
    # Horizontal line for visual separation 
    tags$hr(),
    
    tabBox(
      title = "Plots",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "250px",
      tabPanel("bar Plot", plotOutput("barPlot", height = "400px")),
      tabPanel("Line Graph", plotOutput("linePlot", height = "400px")),
      tabPanel("Scatter Plot", plotOutput("scatterPlot", height = "400px"))),
    
    # Add line break here
    br(),
    # Horizontal line for visual separation 
    tags$hr(),
    
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
  
  output$linePlot <- renderPlot({
    ggplot(data = drivers_subset(),aes(x=Year, y=drivers_sum, group=Gender, color=Cohort)) + 
      geom_line(aes(group = Cohort)) + 
      labs(title="Line Graph") + 
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = "Year", y = "Drivers") + 
      geom_point() + 
      scale_y_continuous(labels = scales::comma) + 
      scale_x_continuous(breaks = seq(1994, 2018, 1))
  })
  
  output$barPlot <- renderPlot({
    ggplot(data = drivers_subset(), aes(x = Year, y = drivers_sum, fill = Cohort)) +
      geom_bar(color = "black",position="stack", stat="identity") + 
      labs(title="Bar Plot") + 
      scale_fill_brewer(palette = "Set3") +
      theme(plot.title = element_text(hjust = 0.5)) + 
      labs(x = "Year", y = "Drivers") + 
      scale_y_continuous(labels = scales::comma) + 
      scale_x_continuous(breaks = seq(1994, 2018, 1))
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(data = drivers_subset())+
      geom_point(aes(x=Year, y=drivers_sum, color=Cohort)) + 
      labs(title="Scatter Plot") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      labs(x = "Year", y = "Drivers") + 
      scale_y_continuous(labels = scales::comma) + 
      scale_x_continuous(breaks = seq(1994, 2018, 1))
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
  
  # Total number of drivers
  output$total_drivers <- renderValueBox({
    valueBox(
      paste0(format(sum(drivers_subset()$drivers_sum), big.mark = ",")), 
      "Total Drivers", 
      icon = icon("car"), 
      color = "blue"
    )
  })
  
  # Total number of female drivers
  output$female_drivers <- renderValueBox({
    valueBox(
      paste0(format(sum(filter(drivers_subset(), Gender == "Female")$drivers_sum), big.mark = ",")), 
      "Female Drivers", 
      icon = icon("female"), 
      color = "green"
    )
  })
  
  # Total number of male drivers
  output$male_drivers <- renderValueBox({
    valueBox(
      paste0(format(sum(filter(drivers_subset(), Gender == "Male")$drivers_sum), big.mark = ",")), 
      "Male Drivers", 
      icon = icon("male"), 
      color = "maroon"
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
