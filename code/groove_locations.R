library(bulletr)
library(dplyr)
library(ggplot2)
library(zoo)
library(readr)

datadir <- "images/Hamby (2009) Barrel/bullets"
all_data <- file.path(datadir, dir(datadir))
knowns <- all_data[grep("[Bb]r[0-9].*", all_data)]
unknowns <- all_data[grep("Ukn*|br[A-Z].*", all_data)]

groove_locations <- lapply(all_data, function(bul) {
    cat(bul, "\n")
    bullet <- read_x3p(bul)
    fortified <- fortify_x3p(bullet)
    all_grooves <- lapply(unique(fortified$x), function(x) {
            cat("Processing", x, "\n")
            cc <- get_crosscut(bullet = bullet, x = x)
            result <- get_grooves(cc, smoothfactor = 25)
            
        return(result$groove)
    })
    
    result <- cbind(data.frame(bullet = bul, x = unique(fortified$x)), do.call(rbind, all_grooves))
    names(result) <- c("bullet", "x", "groove_left", "groove_right")
    
    return(result)
})

groove.locs <- do.call(rbind, groove_locations)
write.csv(groove.locs, file = "csvs/grooves.csv", row.names = FALSE)

grooves <- read_csv("csvs/grooves.csv")
crosscuts <- read_csv("csvs/crosscuts.csv")
apply(crosscuts, 1, function(row) {
    cat(row[1])
    mybullet <- get_crosscut(row[1], x = as.numeric(row[2]))
    
    grooves_sub <- subset(grooves, bullet == row[1])
    
    myind <- which.min(abs(grooves_sub$x - as.numeric(row[2])))
    
    mygroove_left <- grooves_sub$groove_left_pred[myind]
    mygroove_right <- grooves_sub$groove_right_pred[myind]
    
    print(
        qplot(y, value, data = mybullet, geom = "line") +
            theme_bw() +
            geom_vline(xintercept = mygroove_left, colour = "red") +
            geom_vline(xintercept = mygroove_right, colour = "red")
    )
    #my.loess <- fit_loess(mybullet, x)
    #print(my.loess$resid)
    value <- readline("Press Enter to Continue, or q to Quit")
    if (value == "q") break;    
})

