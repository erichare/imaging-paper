library(shiny)
library(x3pr)
library(x3prplus)
library(plotly)
library(gridExtra)
library(zoo)

source(system.file("gui/view", "helpers.R", package = "x3pr"))

Sys.setenv("plotly_username" = "erichare")
Sys.setenv("plotly_api_key" = "xd0oxpeept")

get_grooves <- function(bullet) {
    left <- subset(bullet, y <= 500)
    right <- subset(bullet, y > 500)
    
    leftsmooth <- c(NA, NA, rollapply(left$value, 5, function(x) mean(x, na.rm = TRUE)), NA, NA)
    rightsmooth <- c(NA, NA, rollapply(right$value, 5, function(x) mean(x, na.rm = TRUE)), NA, NA)
    
    final.left <- left[which.min(leftsmooth[-(1:100)]) + 100,]
    final.right <- right[which.min(rightsmooth[-((length(rightsmooth) - 99):length(rightsmooth))]),]
    
    p <- qplot(data = bullet, y, value) +
        theme_bw() + coord_equal() +
        geom_vline(xintercept = final.left$y, colour = "red") +
        geom_vline(xintercept = final.right$y, colour = "blue")
    
    return(list(groove = c(final.left$y, final.right$y), plot = p))
}

fit_loess <- function(bullet, groove) {
    
    bullet_filter <- subset(bullet, !is.na(value) & y > groove$groove[1] & y < groove$groove[2])
    my.loess <- loess(value ~ y, data = bullet_filter)
    bullet_filter$fitted <- fitted(my.loess)
    bullet_filter$resid <- resid(my.loess)
    
    #qplot(data = bullet_filter, y, value) +
    #    theme_bw() + coord_equal() +
    #    geom_smooth()
    
    qplot(data = bullet_filter, y, resid) +
        theme_bw()
}

shinyServer(function(input, output, session) {
    
    bullet1 <- reactive({
        if (is.null(input$file1)) return(read.x3p("images/Hamby252_3DX3P1of2/Br1 Bullet 2-5.x3p"))
        else return(read.x3p(input$file1$datapath))
    })
    
    bullet2 <- reactive({
        if (is.null(input$file2)) return(read.x3p("images/Hamby252_3DX3P1of2/Br1 Bullet 1-3.x3p"))
        else return(read.x3p(input$file2$datapath))
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
        
        p1 <- fit_loess(values$fort1_fixed, b1.groove)
        p2 <- fit_loess(values$fort2_fixed, b2.groove)
        
        grid.arrange(p1, p2)
    })
    
})
