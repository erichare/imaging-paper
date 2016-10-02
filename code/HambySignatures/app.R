library(shiny)

crosscuts <- read.csv("~/GitHub/imaging-paper/csvs/crosscuts.csv")
grooves <- read.csv("~/GitHub/imaging-paper/csvs/grooves.csv")
bullet_stats <- read.csv("~/GitHub/imaging-paper/data-25-25/bullet-stats.csv")

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
