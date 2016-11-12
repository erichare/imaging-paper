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

# Br1 Bullet 2-3.x3p
# Br6 Bullet 2-1.x3p
# Br9 Bullet 2-4.x3p
# Ukn Bullet B-2.x3p
# Ukn Bullet Q-4.x3p
# Ukn Bullet Y-1.x3p

knowns <- all_bullets[c(1:8, 10:78, 80:117, 119:120)]
unknowns <- all_bullets[c(121, 123:177, 179:198, 200:210)]
bullets_processed <- lapply(c(knowns, unknowns), function(bul) {
    cat("Computing processed bullet", basename(bul$path), "\n")
    
    xval <- ccs$cc[which(ccs$path == bul$path)]
    
    grooves_sub <- filter(grooves, bullet == bul$path)
    
    left <- grooves_sub$groove_left_pred[which.min(abs(xval - grooves_sub$x))]
    right <- grooves_sub$groove_right_pred[which.min(abs(xval - grooves_sub$x))]
    
    processBullets(bullet = bul, name = bul$path, x = grooves_sub$x[which.min(abs(xval - grooves_sub$x))], grooves = c(left, right))
})
names(bullets_processed) <- as.character(ccs$path[c(1:8, 10:78, 80:117, 119:120, 121, 123:177, 179:198, 200:210)])

bullets_smoothed <- bullets_processed %>% 
    bind_rows %>%
    bulletSmooth

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
