library(shiny)
library(XML)
library(x3prplus)
library(plotly)
library(gridExtra)
library(zoo)

source(system.file("gui/view", "helpers.R", package = "x3pr"))

Sys.setenv("plotly_username" = "erichare")
Sys.setenv("plotly_api_key" = "xd0oxpeept")

shinyServer(function(input, output, session) {
    
    bullet1 <- reactive({
        if (is.null(input$file1)) return(x3pr::read.x3p("images/Hamby252_3DX3P1of2/Br1 Bullet 2-5.x3p"))
        else return(x3pr::read.x3p(input$file1$datapath))
    })
    
    bullet2 <- reactive({
        if (is.null(input$file2)) return(x3pr::read.x3p("images/Hamby252_3DX3P1of2/Br1 Bullet 1-3.x3p"))
        else return(x3pr::read.x3p(input$file2$datapath))
    })
    
    theSurface <- reactive({
        if (is.null(bullet1()) || is.null(bullet2())) return(NULL)
        
        b1 <- bullet1()
        b2 <- bullet2()
        
        surf.b1 <- b1[[2]]
        surf.b2 <- b2[[2]]
        
        minrows <- min(nrow(surf.b1), nrow(surf.b2))
        
        surf.mat <- cbind(surf.b1[1:minrows,], surf.b2[1:minrows,])
        
        x_idx <- seq(1, nrow(surf.mat), by = input$subsample)
        y_idx <- seq(1, ncol(surf.mat), by = input$subsample)
        
        return(surf.mat[x_idx, y_idx])
    })
    
    observe({
        updateSliderInput(session, "xcoord", max = ncol(theSurface()) / 2 * input$subsample)
    })
    
    output$trendPlot <- renderPlotly({
        if (is.null(theSurface())) return(NULL)
        
        p <- plot_ly(z = theSurface(), type = "surface", showscale = FALSE, lighting = list(ambient = input$ambient_lighting,
                                                                                             diffuse = input$diffuse_lighting,
                                                                                             specular = input$specular_lighting,
                                                                                             roughness = input$roughness_lighting,
                                                                                             fresnel = input$fresnel_lighting))
        p
    })
    
    values <- reactiveValues(fort1_fixed = NULL,
                             fort2_fixed = NULL)
    
    observeEvent(input$compute, {
        fort1 <- fortify_x3p(bullet1())
        fort2 <- fortify_x3p(bullet2())
        
        fixedcoord1 <- input$xcoord %% (length(unique(fort1$x)) / input$subsample)
        fixedcoord2 <- input$xcoord %% (length(unique(fort2$x)) / input$subsample)
        
        ind1 <- which.min(abs(fort1$x - fixedcoord1))
        ind2 <- which.min(abs(fort2$x - fixedcoord2))
        
        values$fort1_fixed <- fort1[fort1$x == fort1[ind1,"x"],]
        values$fort2_fixed <- fort2[fort2$x == fort2[ind2,"x"],]
    })
    
    output$residuals <- renderPlot({
        if (is.null(values$fort1_fixed) || is.null(values$fort2_fixed)) return(NULL)
        
        b1.groove <- get_grooves(values$fort1_fixed)
        b2.groove <- get_grooves(values$fort2_fixed)
        
        l1 <- fit_loess(values$fort1_fixed, b1.groove)
        l2 <- fit_loess(values$fort2_fixed, b2.groove)
        
        my.dat <- data.frame(y = c(l1$data$y, l2$data$y),
                        resid = c(l1$data$resid, l2$data$resid),
                        bullet = factor(c(rep(1, length(l1$data$y)), rep(2, length(l2$data$y)))))
        
        qplot(y, resid, data = my.dat, geom = "line", colour = bullet, group = bullet, size = I(1.3), alpha = I(0.8)) +
            theme_bw() +
            theme(legend.position = "bottom")
    })
    
})
