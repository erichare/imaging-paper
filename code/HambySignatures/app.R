library(shiny)

crosscuts <- read.csv("~/GitHub/imaging-paper/csvs/crosscuts.csv")
grooves <- read.csv("~/GitHub/imaging-paper/csvs/grooves.csv")

ui <- fluidPage(
   
   titlePanel("Hamby Signatures"),
   
   sidebarLayout(
      sidebarPanel(
      ),
      
      mainPanel(
      )
   )
)

server <- function(input, output) {
   
}

shinyApp(ui = ui, server = server)
