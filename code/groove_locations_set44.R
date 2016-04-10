library(x3pr)
library(x3prplus)
library(dplyr)
library(ggplot2)
library(zoo)

knowndatadir <- "~/Downloads/Hamby Set 44/known"
knowns <- file.path(knowndatadir, dir(knowndatadir, pattern="x3p"))

unknowndatadir <- "~/Downloads/Hamby Set 44/unknown"
unknowns <- file.path(unknowndatadir, dir(unknowndatadir))

groove_locations_means <- lapply(c(knowns, unknowns), function(bul) {
    cat(bul, "\n")
    bullet <- read.x3pplus(bul, transpose = TRUE)
    fortified <- fortify_x3p(bullet)
    
    fort.mean <- fortified %>%
        filter(x >= 100, x <= 300) %>%
        group_by(y) %>%
        summarise(value = mean(value, na.rm = TRUE))
    
    cc <- cbind(x = NA, fort.mean)
    result <- get_grooves(cc, smoothfactor = 15, adjust = 0)

    return(result$groove)
})
groove.means <- cbind(data.frame(bullet = c(knowns, unknowns), do.call(rbind, groove_locations_means)))
names(groove.means) <- c("bullet", "groove_left", "groove_right")
write.csv(groove.means, file = "csvs/grooves-mean-set44.csv", row.names = FALSE)

groove.means <- read.csv("csvs/grooves-mean-set44.csv")
mean_window <- 150

lapply(as.data.frame(t(groove.means)), function(test) {
    bul <- as.character(test[1])
    left <- as.numeric(as.character(test[2]))
    right <- as.numeric(as.character(test[3]))
    
    cat(bul, "\n")
    bullet <- read.x3pplus(bul)
    cc <- get_crosscut(bullet = bullet, x = 150)
    result <- get_grooves(cc, mean_left = left, mean_right = right, mean_window = mean_window, smoothfactor = 25)
    
    print(result$plot)

    value <- readline("Press Enter to Continue, or q to Quit")
    if (value == "q") break;    
})
