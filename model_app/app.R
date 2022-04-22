library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  
  # background
  setBackgroundColor("#171717"),
  tags$style(HTML('* {font-family: "Courier new"};')),
  div(style = "color: #20C20E;", h2("Build your linear model"), align = "center"),
  
  # inputs
  selectInput("dataset", label = tags$span(style = "color: #20C20E;", "Choose the dataset"),
              choices = ls("package:datasets"), selected = "mtcars"),
  selectInput("y", label = tags$span(style = "color: #20C20E;", "Dependent variable"), choices = NULL),
  selectInput("x", label = tags$span(style = "color: #20C20E;", "Independent variables"), choices = NULL, multiple = TRUE),
  
  # outputs
  withMathJax(),
  equatiomatic::eqOutput("equation"),
  tags$style("#equation{color: #20C20E;}"),
  
  plotOutput("plot"),
  verbatimTextOutput("summary"),
  tags$style("#summary{color: white;}"),
  tags$style("#summary{background-color: #171717;}")
)

server <- function(input, output, session) {

  # reactive functions
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  
  observe({
    lstname <- names(dataset())
    updateSelectInput(inputId = "y", choices = lstname)
    updateSelectInput(inputId = "x", choices = lstname)
  })
  
  linear_model <- reactive({
    req({input$x})
    lm(as.formula(paste(input$y, "~", paste(input$x, collapse = " + "))), data = dataset())
  })
  
  # outputs
  output$equation <- equatiomatic::renderEq(equatiomatic::extract_eq(linear_model()))
  
  output$plot <- renderPlot({
    par(mfrow = c(2,2), 
        pch = 20,
        bg = "#171717",
        col.axis = "white",
        col.lab = "white",
        col.main = "white",
        fg = "white")
     plot(linear_model(), col = "#20C20E")
   })
  
  output$summary <- renderPrint(summary(linear_model()))
  
}

shinyApp(ui, server)