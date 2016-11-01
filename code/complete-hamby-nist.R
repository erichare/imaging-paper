library(x3pr)
library(x3prplus)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(zoo)

datadir <- "images/Hamby (2009) Barrel/bullets"

# match unknown land using crosscuts
ccs <- read.csv("csvs/crosscuts.csv")
all_bullets <- lapply(as.character(ccs$path), function(x) {
    transpose <- (length(grep(" ", x)) == 0)
    
    result <- read.x3pplus(x, transpose = transpose)
    result[[3]] <- x
    names(result)[3] <- "path"
    
    return(result)
})

grooves <- read.csv("csvs/grooves.csv")

knowns <- all_bullets[1:120]
unknowns <- all_bullets[121:210]
bullets_processed <- lapply(all_bullets, function(bul) {
    cat("Computing processed bullet", basename(bul$path), "\n")
    
    xval <- ccs$cc[which(ccs$path == bul$path)]
    
    grooves_sub <- filter(grooves, bullet == bul$path)
    
    left <- grooves_sub$groove_left[which.min(abs(xval - grooves_sub$x))]
    right <- grooves_sub$groove_right[which.min(abs(xval - grooves_sub$x))]
    
    processBullets(bullet = bul, name = bul$path, x = grooves_sub$x[which.min(abs(xval - grooves_sub$x))], grooves = c(left, right))
})
names(bullets_processed) <- as.character(ccs$path)

bullets_smoothed <- bullets_processed %>% bind_rows

#for (span in c(1)) {
    dataStr <- "data/data-nist" # using crosscuts-25.csv
    
    if (!file.exists(dataStr)) dir.create(dataStr)
    for (j in 1:length(unknowns)) {
        reslist <- lapply(knowns, function(x) {
            cat("Processing", j, "vs", basename(x$path), "\n")
            
            br1 <- filter(bullets_smoothed, bullet == x$path)
            br2 <- filter(bullets_smoothed, bullet == unknowns[[j]]$path)
            
            bulletGetMaxCMS_nist(br1, br2, column = "resid", span=span)
        })
        save(reslist, file=file.path(dataStr, sprintf("unkn%d-nist.RData", j)))
    }
#}
