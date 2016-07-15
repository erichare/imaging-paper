library(shiny)
library(x3pr)
library(x3prplus)
library(ggplot2)
library(smoother)
library(changepoint)

mybullet <- read.x3pplus("~/GitHub/imaging-paper/app/images/Hamby252_3DX3P1of2/Br1 Bullet 1-1.x3p")

mycc <- get_crosscut(x = 100, bullet = mybullet)
my.groove <- get_grooves(mycc)
my.loess <- fit_loess(mycc, my.groove)

ui <- fluidPage(
   
   titlePanel("Exploring Bullet Signature Filtering"),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("long",
                     "Long Window",
                     min = 2,
                     max = 1000,
                     value = 250),
         
         sliderInput("short",
                     "Short Window",
                     min = 2,
                     max = 100,
                     value = 25),
         
         checkboxInput("striae", "Show Striae"),
         
         hr(),
         helpText("Old Striae Detected:"), textOutput("olddetected"),
         
         helpText("New Striae Detected:"), textOutput("detected")
      ),
      
      mainPanel(
         plotOutput("unfiltered", height = "250px"),
         plotOutput("sigplot", height = "250px"),
         plotOutput("newsigplot", height = "250px")
      )
   )
)

server <- function(input, output) {
    
    output$unfiltered <- renderPlot({
        qplot(y, resid, data = my.loess$data, geom = "line") + theme_bw()
    })
    
    oldpeaks <- reactive({
        get_peaks(my.loess$data, striae = input$striae, window = FALSE)
    })
   
   output$sigplot <- renderPlot({
       oldpeaks()$plot
   })
   
   smoothdata <- reactive({
       first <- smth.gaussian(my.loess$data$resid, window = input$short / 1.5625)
       second <- first - smth.gaussian(my.loess$data$resid, window = input$long / 1.5625)
       
       return(second)
   })
   
   output$olddetected <- renderText({
       return(length(c(oldpeaks()$peaks, oldpeaks()$valleys)))
   })
   
   output$detected <- renderText({
       my.inds <- which(diff(sign(diff(smoothdata()))) == -2) + 1
       my.inds2 <- which(diff(sign(diff(smoothdata()))) == 2) + 1
       
       return(length(c(my.inds, my.inds2)))
   })
   
   output$newsigplot <- renderPlot({
       mydat <- smoothdata()
       my.loess$data$smoothed <- mydat
       
       my.inds <- which(diff(sign(diff(smoothdata()))) == -2) + 1
       my.inds2 <- which(diff(sign(diff(smoothdata()))) == 2) + 1
       
       p <- qplot(y, smoothed, data = my.loess$data, geom = "line") + theme_bw()
       
       if (input$striae) p <- p + geom_vline(xintercept = my.loess$data$y[my.inds], colour = "red") + geom_vline(xintercept = my.loess$data$y[my.inds2], colour = "blue")

        return(p)
   })
}

shinyApp(ui = ui, server = server)
