library(x3pr)
library(x3prplus)
library(dplyr)
library(ggplot2)
library(zoo)

knowndatadir <- "app/images/Hamby252_3DX3P1of2/"
knowns <- file.path(knowndatadir, dir(knowndatadir, pattern="x3p"))

unknowndatadir <- "app/images/Hamby252_3DX3P2of2/"
unknowns <- file.path(unknowndatadir, dir(unknowndatadir))

groove_locations_means <- lapply(c(knowns, unknowns), function(bul) {
    transpose <- (length(grep(" ", bul)) == 0)
    
    cat(bul, "\n")
    bullet <- read.x3pplus(bul, transpose = transpose)
    fortified <- fortify_x3p(bullet)
    
    fort.mean <- fortified %>%
        filter(x >= 100, x <= 300) %>%
        group_by(y) %>%
        summarise(value = mean(value, na.rm = TRUE))
    
    cc <- cbind(x = NA, fort.mean)
    result <- get_grooves(cc, smoothfactor = 25, adjust = 0)

    return(result$groove)
})
groove.means <- cbind(data.frame(bullet = c(knowns, unknowns), do.call(rbind, groove_locations_means)))
names(groove.means) <- c("bullet", "groove_left", "groove_right")
write.csv(groove.means, file = "csvs/grooves-mean.csv", row.names = FALSE)

groove.means <- read.csv("csvs/grooves-mean.csv")
mean_window <- 150

lapply(as.data.frame(t(groove.means)), function(test) {
    bul <- as.character(test[1])
    left <- as.numeric(as.character(test[2]))
    right <- as.numeric(as.character(test[3]))
    
    transpose <- (length(grep(" ", bul)) == 0)
    
    cat(bul, "\n")
    bullet <- read.x3pplus(bul, transpose = transpose)
    cc <- get_crosscut(bullet = bullet, x = 150, transpose = transpose)
    result <- get_grooves(cc, mean_left = left, mean_right = right, mean_window = mean_window, smoothfactor = 25)
    
    print(result$plot)

    value <- readline("Press Enter to Continue, or q to Quit")
    if (value == "q") break;    
})

groove_locations <- lapply(as.data.frame(t(groove.means)), function(test) {
    bul <- as.character(test[1])
    left <- as.numeric(as.character(test[2]))
    right <- as.numeric(as.character(test[3]))
    
    transpose <- (length(grep(" ", bul)) == 0)
    
    cat(bul, "\n")
    bullet <- read.x3pplus(bul, transpose = transpose)
    fortified <- fortify_x3p(bullet)
    all_grooves <- lapply(unique(fortified$x), function(x) {
        myresult <- try({
            cat("Processing", x, "\n")
            cc <- get_crosscut(bullet = bullet, x = x, transpose = transpose)
            result <- get_grooves(cc, mean_left = left, mean_right = right, mean_window = mean_window, smoothfactor = 25)
            
            result$groove
        })

        if (inherits(myresult, "try-error")) return(c(left, right))
        
        return(myresult)
    })
    
    result <- cbind(data.frame(bullet = bul, x = unique(fortified$x)), do.call(rbind, all_grooves))
    names(result) <- c("bullet", "x", "groove_left", "groove_right")
    
    return(result)
})

groove.locs <- do.call(rbind, groove_locations)
write.csv(groove.locs, file = "grooves.csv", row.names = FALSE)
