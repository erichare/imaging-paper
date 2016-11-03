library(x3pr)
library(x3prplus)
library(ggplot2)
library(dplyr)

load("~/GitHub/x3prplus/data/rf.RData")

cary_paths <- file.path("../images/Cary Persistence/bullets", dir("../images/Cary Persistence/bullets"))

cary1 <- read.x3pplus("../images/Cary Persistence/bullets/CWBLT0001-1.x3p")
cary1_fort <- fortify_x3p(cary1)

myx <- bulletCheckCrossCut(path = cary_paths[1], bullet = cary1)
cary1_cross <- get_crosscut(path = cary_paths[1], bullet = cary1)
cary1_groove <- get_grooves(cary1_cross)

ggplot(data = cary1_cross, aes(x = y, y = value)) + geom_line() + theme_bw()
cary1_groove$plot

cary1_loess <- fit_loess(bullet = cary1_cross, groove = list(groove = cary1_groove$groove), span = .03)
cary1_processed <- processBullets(bullet = cary1, name = "cary1", x = myx, grooves = cary1_groove$groove)


other_carys <- lapply(cary_paths[-1], function(x) {
    result <- read.x3pplus(x)
    result[[3]] <- x
    names(result)[3] <- "path"
    
    return(result)
})

carys_processed <- lapply(other_carys, function(bul) {
    cat("Computing processed bullet", basename(bul$path), "\n")
    
    myx <- bulletCheckCrossCut(path = bul$path, bullet = bul)
    if (is.na(myx)) myx <- 200
    
    cary_cross <- get_crosscut(path = bul$path, bullet = bul)
    cary_groove <- get_grooves(cary_cross, smoothfactor = 35)

    processBullets(bullet = bul, name = bul$path, x = myx, grooves = cary_groove$groove)
})
names(carys_processed) <- cary_paths[-1]

