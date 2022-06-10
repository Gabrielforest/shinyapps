library( shiny )

ui <- fluidPage(
  includeHTML( "widget.html" )
)

server <- function( input, output, session ) { }

shinyApp( ui, server )