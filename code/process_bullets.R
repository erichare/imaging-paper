library(x3pr)
library(x3prplus)
library(scatterplot3d)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(zoo)
library(gridExtra)
library(plotly)

Sys.setenv("plotly_username" = "erichare")
Sys.setenv("plotly_api_key" = "xd0oxpeept")

get_bullet <- function(path, x = 99.84) {
    br111 <- read.x3p(path)
    dbr111 <- fortify_x3p(br111)
    
    dbr111.fixx <- dbr111[dbr111$x == x,]
    
    return(dbr111.fixx)
}

get_grooves <- function(bullet) {
    left <- subset(bullet, y <= 500)
    right <- subset(bullet, y > 500)
    
    leftsmooth <- c(NA, NA, rollapply(left$value, 5, function(x) mean(x, na.rm = TRUE)), NA, NA)
    rightsmooth <- c(NA, NA, rollapply(right$value, 5, function(x) mean(x, na.rm = TRUE)), NA, NA)
    
    final.left <- left[which.min(leftsmooth[-(1:100)]) + 100,]
    final.right <- right[which.min(rightsmooth[-((length(rightsmooth) - 99):length(rightsmooth))]),]
    
    p <- qplot(data=bullet, y, value) +
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
    
    # filter out most extreme residuals
    bullet_filter$abs_resid <-  abs(bullet_filter$resid)
    cutoff <- quantile(bullet_filter$abs_resid, probs = c(0.995))
    bullet_filter$chop <- bullet_filter$abs_resid > cutoff

    qplot(data = bullet_filter, y, resid, colour = chop) +
      theme_bw() 
    bullet_filter <- subset(bullet_filter, chop != TRUE)
    
    qplot(data = bullet_filter, y, value) +
        theme_bw() + coord_equal() +
        geom_smooth()
    
    qplot(data = bullet_filter, y, resid, geom="line") +
        theme_bw()
}

plot_3d_land <- function(path, bullet, groove, x = 99.84) {
    br111 <- read.x3p(file.path("images", "Hamby252_3DX3P1of2", "Br1 Bullet 1-1.x3p"))
    inds <- which(bullet$y > groove$groove[1] & bullet$y < groove$groove[2])
    surfmat <- br111$surface.matrix

    plot_ly(z = surfmat[inds,], type = "surface")
}

list_of_plots <- lapply(dir("images/Hamby252_3DX3P1of2"), function(file) {
    br111 <- get_bullet(file.path("images", "Hamby252_3DX3P1of2", file))
    br111.groove <- get_grooves(br111)
    br111.groove$plot
    fit_loess(br111, br111.groove)
})

grid.arrange(list_of_plots[[1]], list_of_plots[[2]], list_of_plots[[3]],
             list_of_plots[[4]], list_of_plots[[5]], list_of_plots[[6]],
             list_of_plots[[7]], list_of_plots[[8]], list_of_plots[[9]],
             list_of_plots[[10]], list_of_plots[[11]], list_of_plots[[12]], ncol = 3)

br111 <- get_bullet(file.path("images", "Hamby252_3DX3P1of2", "Br1 Bullet 1-1.x3p"))
br111.groove <- get_grooves(br111)
plot_3d_land(file.path("images", "Hamby252_3DX3P1of2", "Br1 Bullet 1-1.x3p"), br111, br111.groove)
