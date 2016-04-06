library(x3pr)
library(x3prplus)
library(ggplot2)
library(dplyr)

grooves <- read.csv("csvs/grooves-mean.csv")

lapply(as.data.frame(t(grooves)), function(x) {
    mybullet <- read.x3pplus(as.character(x[1]))
    fortified <- fortify_x3p(mybullet)
    
    cc <- fortified %>%
        group_by(y) %>%
        summarise(value = mean(value, na.rm = TRUE))
    
    cc2 <- get_crosscut(bullet = mybullet, x = 136)
    
    print(qplot(y, value, data = cc, geom = "line",colour = I("red")) +
        theme_bw() +
        geom_vline(xintercept = as.numeric(as.character(x[2])), colour = I("blue")) +
        geom_vline(xintercept = as.numeric(as.character(x[3])), colour = I("blue")) +
        geom_line(data = cc2, alpha = I(0.8)))
    
    value <- readline("Press Enter to Continue, or q to Quit")
    if (value == "q") break;
})
