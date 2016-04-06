library(x3pr)
library(x3prplus)
library(dplyr)
library(ggplot2)
library(zoo)

knowndatadir <- "app/images/Hamby252_3DX3P1of2/"
knowns <- file.path(knowndatadir, dir(knowndatadir, pattern="x3p"))

unknowndatadir <- "app/images/Hamby252_3DX3P2of2/"
unknowns <- file.path(unknowndatadir, dir(unknowndatadir))

groove_locations <- lapply(c(knowns, unknowns), function(bul) {
    cat(bul, "\n")
    bullet <- read.x3pplus(bul)
    fortified <- fortify_x3p(bullet)
    all_grooves <- lapply(unique(fortified$x), function(x) {
        cat("Processing", x, "\n")
        cc <- get_crosscut(bullet = bullet, x = x)
        result <- get_grooves(cc, smoothfactor = 15)
        
        return(result$groove)
    })
    
    result <- cbind(data.frame(bullet = bul, x = unique(fortified$x)), do.call(rbind, all_grooves))
    names(result) <- c("bullet", "x", "groove_left", "groove_right")
    
    return(result)
})

groove.locs <- do.call(rbind, groove_locations)
write.csv(groove.locs, file = "grooves.csv", row.names = FALSE)

groove_locations_means <- lapply(c(knowns, unknowns), function(bul) {
    cat(bul, "\n")
    bullet <- read.x3pplus(bul)
    fortified <- fortify_x3p(bullet)
    
    fort.mean <- fortified %>%
        group_by(y) %>%
        summarise(value = mean(value, na.rm = TRUE))
    
    cc <- cbind(x = NA, fort.mean)
    result <- get_grooves(cc, smoothfactor = 15)

    return(result$groove)
})
groove.means <- cbind(data.frame(bullet = c(knowns, unknowns), do.call(rbind, groove_locations_means)))
names(groove.means) <- c("bullet", "groove_left", "groove_right")
write.csv(groove.means, file = "csvs/grooves-mean.csv", row.names = FALSE)
