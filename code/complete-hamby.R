library(x3pr)
library(x3prplus)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(zoo)

knowndatadir <- "app/images/Hamby252_3DX3P1of2/"
unknowndatadir <- "app/images/Hamby252_3DX3P2of2/"

# match unknown land using crosscuts
ccs <- read.csv("csvs/crosscuts-25.csv")
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
    left <- grooves$groove_left[grooves$bullet == bul$path & grooves$x == xval]
    right <- grooves$groove_right[grooves$bullet == bul$path & grooves$x == xval]
    
    processBullets(bullet = bul, name = bul$path, x = xval, grooves = c(left, right))
})
names(bullets_processed) <- as.character(ccs$path)

bullets_smoothed <- bullets_processed %>% bind_rows %>% bulletSmooth

for (span in c(25)) {
    dataStr <- sprintf("data-%d-25", span) # using crosscuts-25.csv
    
    if (!file.exists(dataStr)) dir.create(dataStr)
    for (j in 1:length(unknowns)) {
        reslist <- lapply(knowns, function(x) {
            cat("Processing", j, "vs", basename(x$path), "with span", span, "\n")
            
            br1 <- filter(bullets_smoothed, bullet == x$path)
            br2 <- filter(bullets_smoothed, bullet == unknowns[[j]]$path)
            
            if (all_bullets[[j]]$path != x$path) {
                bulletGetMaxCMS(br1, br2, span=span)
            }
        })
        save(reslist, file=file.path(dataStr, sprintf("unkn%d-new.RData", j)))
    }
}
