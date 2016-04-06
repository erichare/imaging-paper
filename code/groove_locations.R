library(x3pr)
library(x3prplus)
library(dplyr)
library(ggplot2)
library(zoo)

knowndatadir <- "app/images/Hamby252_3DX3P1of2/"
knowns <- file.path(knowndatadir, dir(knowndatadir, pattern="x3p"))

unknowndatadir <- "app/images/Hamby252_3DX3P2of2/"
unknowns <- file.path(unknowndatadir, dir(unknowndatadir))

crosscuts <- lapply(c(knowns, unknowns), function(bul) {
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
