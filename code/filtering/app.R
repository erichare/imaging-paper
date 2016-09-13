library(shiny)
library(x3pr)
library(x3prplus)
library(ggplot2)
library(smoother)
library(changepoint)

## (Hold off on this) Power spectral density (in Matlab, psd function) (like kernel smoothing with an adaptive window)
## High/low pass filters.
## Target those frequencies 
## Rather than gaussian, use local linear polynomial regression
## Possibly need a different filter for each signature
## Boundary effects: mirroring / data sharpening for bias reduction. reflect across boundaries on both sides
## Smooth across whole thing but dont extrapolate
## Significant Zero Crossings of the Derivative SiZeR (JASA paper Chu Marron) - R Package SiZeR
## Interpolating spline > loess - use this to get the bullet signatures. Smooth with local polynomial
## Total Variation (Try this after the alignment step)

mybullet <- read.x3pplus("~/GitHub/imaging-paper/app/images/Hamby252_3DX3P1of2/Br1 Bullet 1-5.x3p")

mycc <- get_crosscut(x = 100, bullet = mybullet)
mycc$value <- mycc$value - min(mycc$value, na.rm = TRUE)
my.groove <- get_grooves(mycc)
my.loess <- fit_loess(mycc, my.groove)

## NEW
mycc_sub <- dplyr::filter(mycc, y >= my.groove$groove[1], y <= my.groove$groove[2])
qplot(y, value, data = mycc_sub, geom = "line") + theme_bw()
mycc_sub$value <- na.fill(mycc_sub$value, "extend")
splinefit <- smooth.spline(mycc_sub$y, mycc_sub$value)
intersplinefit <- spline(mycc_sub$y, mycc_sub$value, method = "natural")
mycc_sub$myspline <- residuals(smooth.spline(mycc_sub$y, mycc_sub$value))
qplot(y, myspline, data = mycc_sub, geom = "line") + theme_bw()
mycc_sub$smoothed <- fitted(loess(myspline ~ y, data = mycc_sub, span = 0.07))
qplot(y, smoothed, data = mycc_sub, geom = "line") + theme_bw()
qplot(intersplinefit$x, intersplinefit$y, geom = "line") + theme_bw()

mybullet2 <- read.x3pplus("~/GitHub/imaging-paper/app/images/Hamby252_3DX3P1of2/Br1 Bullet 2-1.x3p")

mycc2 <- get_crosscut(x = 100, bullet = mybullet2)
mycc2$value <- mycc2$value - min(mycc2$value, na.rm = TRUE)
my.groove2 <- get_grooves(mycc2)

mycc_sub2 <- dplyr::filter(mycc2, y >= my.groove2$groove[1], y <= my.groove2$groove[2])
qplot(y, value, data = mycc_sub2, geom = "line") + theme_bw()
mycc_sub2$value <- na.fill(mycc_sub2$value, "extend")
mycc_sub2$myspline <- residuals(smooth.spline(mycc_sub2$y, mycc_sub2$value))
qplot(y, myspline, data = mycc_sub2, geom = "line") + theme_bw()
intersplinefit2 <- spline(mycc_sub2$y, mycc_sub2$value, method = "natural")
qplot(intersplinefit2$x, intersplinefit2$y, geom = "line") + theme_bw()

mydf_interp <- data.frame(bullet = c(rep("b1", length(intersplinefit$x)), rep("b2", length(intersplinefit2$x))),
                   y = c(intersplinefit$x, intersplinefit2$x),
                   l30 = c(intersplinefit$y, intersplinefit2$y))

mycc_sub2$smoothed <- fitted(loess(myspline ~ y, data = mycc_sub2, span = 0.07))
qplot(y, smoothed, data = mycc_sub2, geom = "line") + theme_bw() + geom_line(data = mycc_sub, aes(x = y, y = smoothed), colour = I("red"))
mydf <- data.frame(bullet = c(rep("b1", nrow(mycc_sub)), rep("b2", nrow(mycc_sub2))),
                   y = c(mycc_sub$y, mycc_sub2$y),
                   l30 = c(mycc_sub$smoothed, mycc_sub2$smoothed))
result <- bulletAlign(mydf)
result2 <- bulletAlign(mydf_interp)

qplot(y, val, data = result$bullets, geom = "line", colour = bullet) + theme_bw()
qplot(y, val, data = result2$bullets, geom = "line", colour = bullet) + theme_bw()

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
