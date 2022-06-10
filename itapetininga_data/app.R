library( shiny ); 

local <- "Itapetininga"
thedata <- read.csv2( "fustes.csv" )
names( thedata ) <- c( "População", "Parcela", "Árvore", "Fuste", "CAP", "Altura", "Observações" )

ui <- fluidPage(
  
  titlePanel( paste0( "Dados Inventário Florestal - ", local ) ),

  sidebarPanel(
    downloadButton( "download", "Download" ),
  ),
  
  mainPanel(
    dataTableOutput( "dto" )
  )
  
)

server <- function( input, output ) {
  
  output$dto <- renderDataTable( { thedata } )

  output$download <- downloadHandler(
    filename = paste0( "dados_inventário_", local, ".csv" ),
    content = function( fname ) { write.csv( thedata, fname ) }
  )
}

shinyApp( ui, server )