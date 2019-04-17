#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Savings/Investing Scenarios"),
   
   # Sidebar with a slider input for number of bins 
   verticalLayout(
         sliderInput("amount",
                     "Initial Amount",
                     min = 0,
                     max = 100000,
                     value = 1000, step = 500),
         sliderInput("contrib",
                     "Annual Cotribution",
                     min = 0,
                     max = 50000,
                     value = 2000, step = 500),
         sliderInput("rate",
                     "Return Rate",
                     min = 0,
                     max = .2,
                     value = .02, step = .001),
         sliderInput("growth",
                     "Growth Rate",
                     min = 0,
                     max = .2,
                     value = .021, step = .001),
         sliderInput("years",
                     "Years",
                     min = 0,
                     max = 50,
                     value = 20, step = 1),
         selectInput('facet', 'Facet?', c('Yes', 'No'), selected = 'No'),
         plotOutput("Timelines"),
         tableOutput("Balances"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$Timelines <- renderPlot({
     future_value <- function(amount, rate, years){
       future <- amount * ((1 + rate)**years)
       return(future)
     }
     annuity <- function(amount, contrib, rate, years){
       fva <- contrib * ((((1 + rate)**years) - 1)/rate) + amount * ((1 + rate)**years)
       return(fva)
     }
     growing_annuity <- function(amount, contrib, rate, growth, years){
       fvga <- contrib*(((1+rate)**years - (1 + growth)**years)/(rate - growth)) + amount * ((1 + rate)**years)
       return(fvga)
     }
     no_contrib <- rep(0, input$years + 1)
     for(i in 1:(input$years + 1)){
       no_contrib[i] <- future_value(input$amount, input$rate, (i - 1)) 
     }
     fixed_contrib <- rep(0, input$years + 1)
     for(i in 1:(input$years + 1)){
       fixed_contrib[i] <- annuity(input$amount, input$contrib, input$rate, (i - 1))
     }
     growing_contrib <- rep(0,input$years + 1)
     for(i in 1:(input$years + 1)){
       growing_contrib[i] <- growing_annuity(input$amount, input$contrib, input$rate, input$growth, (i - 1))
     }
     year <- (0:input$years)
     modalities <- data.frame(year, no_contrib, fixed_contrib, growing_contrib)
     if (input$facet == 'No'){
        ggplot(modalities) + geom_line(aes(x = year, y = no_contrib, col = 'red1')) + geom_line(aes(x = year, y = fixed_contrib, col = 'green1')) + geom_line(aes(x = year, y = growing_contrib, col = 'blue1')) + xlab('Years') + ylab('Savings') + ggtitle('Three Modes of Investing') + scale_color_manual(values = c('red1', 'green1', 'blue1'), name = 'variable', labels = c('growing_contrib', 'fixed_contrib', 'no_contrib'))
     } else {
       ggplot(modalities) + geom_line(aes(x = year, y = no_contrib, col = 'red1')) + geom_line(aes(x = year, y = fixed_contrib, col = 'green1')) + geom_line(aes(x = year, y = growing_contrib, col = 'blue1')) + xlab('Years') + ylab('Savings') + ggtitle('Three Modes of Investing') + scale_color_manual(values = c('red1', 'green1', 'blue1'), name = 'variable', labels = c('growing_contrib', 'fixed_contrib', 'no_contrib')) + facet_grid()
     }
   })
  output$Balances <- renderTable({
    future_value <- function(amount, rate, years){
      future <- amount * ((1 + rate)**years)
      return(future)
    }
    annuity <- function(amount, contrib, rate, years){
      fva <- contrib * ((((1 + rate)**years) - 1)/rate) + amount * ((1 + rate)**years)
      return(fva)
    }
    growing_annuity <- function(amount, contrib, rate, growth, years){
      fvga <- contrib*(((1+rate)**years - (1 + growth)**years)/(rate - growth)) + amount * ((1 + rate)**years)
      return(fvga)
    }
    no_contrib <- rep(0, input$years + 1)
    for(i in 1:(input$years + 1)){
      no_contrib[i] <- future_value(input$amount, input$rate, (i - 1)) 
    }
    fixed_contrib <- rep(0, input$years + 1)
    for(i in 1:(input$years + 1)){
      fixed_contrib[i] <- annuity(input$amount, input$contrib, input$rate, (i - 1))
    }
    growing_contrib <- rep(0,input$years + 1)
    for(i in 1:(input$years + 1)){
      growing_contrib[i] <- growing_annuity(input$amount, input$contrib, input$rate, input$growth, (i - 1))
    }
    year <- (0:input$years)
    modalities <- data.frame(year, no_contrib, fixed_contrib, growing_contrib)
    modalities
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

