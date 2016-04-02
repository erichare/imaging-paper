library(ggplot2)
library(x3pr)
library(x3prplus)
library(dtw)
library(dplyr)

get_alignment <- function(b1, b2) {
    bb1 <- fortify_x3p(read.x3p(b1))
    bb2 <- fortify_x3p(read.x3p(b2))
    
    b1sub <- subset(bb1, x == 100)
    b2sub <- subset(bb2, x == 100)
    
    regular <- b1sub$value
    degraded <- b2sub$value
    
    regular_nona <- regular
    regular_nona[is.na(regular_nona)] <- 0
    degraded_nona <- degraded
    degraded_nona[is.na(degraded_nona)] <- 0
    
    #qplot(y, value, data = b1sub, colour = I("red"), geom = "line") + theme_bw() +
    #    geom_line(data = b2sub, aes(x = y, y = value), colour = I("blue"))
    
    alignment <- dtw(regular_nona, degraded_nona)
    
    mapping <- data.frame(index1 = alignment$index1, index2 = alignment$index2)
    mapping <- mapping %>%
        group_by(index1) %>%
        summarise(index2_new = max(index2))
    result <- mapping %>% group_by(index2_new) %>% summarise(index1 = tail(index1, n = 1))
    mydiffs <- apply(result, 1, diff)
    
    #qplot(b1sub$y, b1sub$value, colour = I("red"), geom = "line") + 
    #   geom_line(aes(x = b2sub$y, y = b2sub$value), colour = I("blue"))
    
    #result <- which.max(table(alignment$index2)[!(names(table(alignment$index2)) %in% tail(sort(unique(alignment$index2))))])
    #index.val <- min.ind <- as.numeric(names(result))
    #min.ind <- tail(which(alignment$index2 == index.val), n = 1)
    #min.ind <- 478
    #min.ind <- min(result$index1)
    min.ind <- as.numeric(names(table(mydiffs))[which.max(table(mydiffs))])
    incr <- diff(b1sub$y[1:2])
    b2new <- b2sub
    b2new$y <- b2sub$y + (incr * min.ind)
    
    qplot(y, value, data = b1sub, colour = I("red"), geom = "line") + theme_bw() +
        geom_line(data = b2new, aes(x = y, y = value), colour = I("blue"))
}

b1s <- paste0("~/GitHub/imaging-paper/app/images/Hamby252_3DX3P1of2/", dir("~/GitHub/imaging-paper/app/images/Hamby252_3DX3P1of2"))
b2s <- paste0("~/GitHub/imaging-paper/app/degraded_images/Hamby252_3DX3P1of2/", dir("~/GitHub/imaging-paper/app/degraded_images/Hamby252_3DX3P1of2"))

b3s <- paste(b1s, b2s, sep = "___")

lapply(b3s, function(b3) {
    cat(b3, "\n")
    test <- strsplit(b3, "___")
    print(get_alignment(test[[1]][1], test[[1]][2]))
    result <- readline("Press Enter to Continue")
    if (result == "q") break;
})
    
matches <- read.csv("csvs/matches.csv", header = FALSE)
b1s <- paste0("~/GitHub/imaging-paper/app/images/Hamby252_3DX3P1of2/", matches$V1, ".x3p")
b2s <- paste0("~/GitHub/imaging-paper/app/images/Hamby252_3DX3P1of2/", matches$V2, ".x3p")

b3s <- paste(b1s, b2s, sep = "___")

lapply(b3s, function(b3) {
    cat(b3, "\n")
    test <- strsplit(b3, "___")
    print(get_alignment(test[[1]][1], test[[1]][2]))
    result <- readline("Press Enter to Continue")
    if (result == "q") break;
})
