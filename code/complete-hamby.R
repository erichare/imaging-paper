library(bulletr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(zoo)

datadir <- "images/Hamby (2009) Barrel/bullets"

# match unknown land using crosscuts
ccs <- read.csv("csvs/crosscuts.csv")
all_bullets <- lapply(as.character(ccs$path), function(x) {
    result <- read_x3p(x)
    result[[3]] <- x
    names(result)[3] <- "path"
        
    return(result)
})
names(all_bullets) <- ccs$path

grooves <- read.csv("csvs/grooves.csv")
toexclude <- ccs$path[is.na(ccs$cc)]
all_bullets <- all_bullets[!(names(all_bullets) %in% toexclude)]

knowns <- all_bullets[grep("[Bb]r[0-9].*", names(all_bullets))]
unknowns <- all_bullets[grep("Ukn*|br[A-Z].*", names(all_bullets))]

bullets_processed <- lapply(c(knowns, unknowns), function(bul) {
    cat("Computing processed bullet", basename(bul$path), "\n")
    
    xval <- ccs$cc[which(ccs$path == bul$path)]
    
    grooves_sub <- filter(grooves, bullet == bul$path)
    
    left <- grooves_sub$groove_left_pred[which.min(abs(xval - grooves_sub$x))]
    right <- grooves_sub$groove_right_pred[which.min(abs(xval - grooves_sub$x))]
    
    processBullets(bullet = bul, name = bul$path, x = grooves_sub$x[which.min(abs(xval - grooves_sub$x))], grooves = c(left, right))
})
names(bullets_processed) <- c(names(knowns), names(unknowns))

bullets_smoothed <- bullets_processed %>% 
    bind_rows %>%
    bulletSmooth()

for (span in c(25)) {
    dataStr <- sprintf("data/data-%d-25", span) # using crosscuts-25.csv
    
    if (!file.exists(dataStr)) dir.create(dataStr)
    for (j in 1:length(unknowns)) {
        reslist <- lapply(knowns, function(x) {
            cat("Processing", j, "vs", basename(x$path), "with span", span, "\n")
            
            br1 <- filter(bullets_smoothed, bullet == x$path)
            br2 <- filter(bullets_smoothed, bullet == unknowns[[j]]$path)
            
            if (all_bullets[[j]]$path != x$path) {
                bulletGetMaxCMS(br1, br2, column = "l30", span=span)
            }
        })
        save(reslist, file=file.path(dataStr, sprintf("unkn%d-new.RData", j)))
    }
}
