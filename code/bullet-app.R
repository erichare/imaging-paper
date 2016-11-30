library(shiny)
library(plotly)
library(bulletr)

hamby_dir <- "../images/Hamby (2009) Barrel/bullets"
cary_dir <- "../images/Cary Persistence/bullets/"

all_images <- c(file.path(hamby_dir, dir(hamby_dir)), file.path(cary_dir, dir(cary_dir)))

ui <- fluidPage(
   
   titlePanel("Bullet Database"),
   
   sidebarLayout(
      sidebarPanel(
          selectInput("land", "Bullet Land", choices = all_images)
      ),
      
      mainPanel(
          plotlyOutput("viewland")
      )
   )
)

server <- function(input, output) {
    mybullet <- reactive({
        read_x3p(input$land)
    })
    
    output$viewland <- renderPlotly({
        plot_3d_land(bullet = mybullet())
    })
}

shinyApp(ui = ui, server = server)
