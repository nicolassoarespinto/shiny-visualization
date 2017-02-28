library(shiny)

ui <-  fluidPage(    
  

  titlePanel("Desenvolvimento Monetário do Brasil no Século XX"),

  
  sidebarLayout(      
    

    sidebarPanel(
      selectInput("ano_inicial", "De:", 
                  choices=names(table(a$ano))), 
      selectInput("ano_final","Até:",
                  choices= names(table(a$ano))),
      hr(),
      helpText("Fonte: IBGE.")
    ),
    # Create a spot for the barplot
    mainPanel(
      plotOutput("plot1")  
    )
    
  )
)

server <- function(input, output) {
  
  
  
  
  create_plot <- reactive({
    
    
    b<- a %>% filter(ano>= input$ano_inicial & ano<=input$ano_final) %>% filter(key %in% c("papel_moeda_emitido", "papel_moeda_pp"))
    
    g<- ggplot(b, 
               aes( x = periodo, y = var, color = moeda)) + 
      geom_path(group = "key", size = 0.5)  + facet_wrap("key")
    
    #############################
    
    g + scale_y_continuous(labels = comma) + 
      ggtitle("Desenvolvimento Monetário do Brasil no Século XX - 1900 - 1930") + 
      theme_bw() +
      theme(title = element_text(size = 10, face = 'bold', family = "Garamond"),
            axis.title = element_blank(),
            axis.text = element_text(family = "Garamond"),
            panel.grid.minor = element_blank())
    g<- g + scale_y_continuous(labels = comma)
    
    return(g)
  })
  
  output$plot1 <- renderPlot({
    
    create_plot()
    
  })
  
}

app <- shinyApp(ui = ui, server = server)

#Rodar App shiny

runApp(app)
