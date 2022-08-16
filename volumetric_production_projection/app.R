library( shiny )
library( shinyWidgets )

ui <- fluidPage(
  
  # background
  setBackgroundColor( "#171717" ),
  tags$style( HTML( '* {font-family: "Courier new"};' ) ),
  div( style = "color: #20C20E;", h2( "Projeção produção volumétrica - Bailey e Clutter Polimórfico" ), align = "center" ),
  
  ## inputs
  fluidRow(
    column( width = 6,
            numericInput( "b0", label = tags$span(style = "color: #20C20E;", "\u03B20" ),
                          min = -50, max = 50, value = 2.087605 ),
            numericInput( "b1", label = tags$span(style = "color: #20C20E;", "\u03B21" ),
                          min = -50, max = 50, value = 18.960836 ),
            numericInput( "b2", label = tags$span(style = "color: #20C20E;", "\u03B22" ),
                          min = -50, max = 50, value = 10.948749 ),
            numericInput( "b3", label = tags$span(style = "color: #20C20E;", "\u03B23" ),
                          min = -50, max = 50, value = 1.368863 ),
            numericInput( "b4", label = tags$span(style = "color: #20C20E;", "\u03B24" ),
                          min = -50, max = 50, value = 5.738665 ),
            numericInput( "b5", label = tags$span(style = "color: #20C20E;", "\u03B25" ),
                          min = -50, max = 50, value = 0.030832 ) ),
    
    column( width = 6,
    sliderInput( "id1", label = tags$span( style = "color: #20C20E;", "Idade de medição (anos)" ),
                 min = 0, max = 5, value = 3.7 ),
    sliderInput( "ab1", label = tags$span( style = "color: #20C20E;", "Área basal (m³/ha)" ),
                 min = 0, max = 60, value = 25.08 ),
    sliderInput( "s", label = tags$span( style = "color: #20C20E;", "Sítio (m)" ),
                 min = 0, max = 60, value = 25.08 ) ) ),
  
  # outputs
  plotOutput( "plot" ),
  tags$style( "#summary{background-color: #171717;}" )
)

server <- function(input, output, session) {
  
  #' Definição da idade ótima de corte silvicultural
  output$plot <- renderPlot({
    b0 <- input$b0
    b1 <- input$b1
    b2 <- input$b2 
    b3 <- input$b3  
    b4 <- input$b4  
    b5 <- input$b5  
    id1 <- input$id1 * 12  
    ab1 <- input$ab1
    s <- input$s
    
    eqvol <- expression( exp( b0 + b1/s + b2/x + b3*( id1/x )*log( ab1 ) + b4*( 1-id1/x ) + b5*( 1-id1/x )*s ) )
    d_eqvol <- as.character( D( eqvol, "x" ) )
    d_eqvol <- paste( d_eqvol[ 2 ], d_eqvol[ 3 ], sep = "*" ) 
    der_eqvol <- parse( text = d_eqvol )
    
    vtcc <- function( x ) ( eval( eqvol ) )
    icm <- function( x )( eval( der_eqvol ) )
    imn <- function( x )( vtcc( x )/( x ) )
    op <- optimize( f = imn, interv = c( 12, 120 ), maximum = T )
    iocs <- op$maximum
    iocs <- iocs/12
    
    par( mfrow = c( 2, 1 ),
         pch = 20,
         mai = c( 0.8, 0.9, 0.3, 0.2 ),
         bg = "#171717",
         col.axis = "white",
         col.lab = "white",
         col.main = "white",
         fg = "white",
         bty = "l" )
    curve( vtcc( x ), 12, 120, xlab = "", ylab = "vtcc (m³/ha)" )
    curve( icm( x )* 12, 12, 120, xlab = "idade (meses)", ylab = "icm (m³/ha.ano)", col = "red" )
    curve( imn( x )* 12, 12, 120, xlab = "idade (meses)", ylab = "ima (m³/ha.ano)", add = TRUE, col = "green" )
    par( xpd = NA )
    abline( v = iocs * 12, col = "white", lty = 2 )
    legend( "topright", c( "ica", "ima", "corte" ), text.col = c( "red", "green", "white" ), box.lty = 0,
            col = c( "red", "green", "white" ), lty = c( 1, 1, 2 ) )
    text( iocs * 12, imn( iocs ) * 12, paste( round( iocs, 2 ), " anos" ), cex = 0.8, col = "white" )
  })
  
}

shinyApp(ui, server)
