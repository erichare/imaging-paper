library(ggplot2)
library(x3pr)
library(x3prplus)
library(dplyr)
    
matches <- read.csv("csvs/matches.csv", header = FALSE)
ccs <- read.csv("csvs/crosscuts-25.csv")

b1s <- paste0("app/images/Hamby252_3DX3P1of2//", matches$V1, ".x3p")
b2s <- paste0("app/images/Hamby252_3DX3P1of2//", matches$V2, ".x3p")
#b2s <- paste0("app/degraded_images/Hamby252_3DX3P1of2//", gsub(" ", "_", matches$V1), ".x3p")

b3s <- paste(b1s, b2s, sep = "___")

bulletAlign_test <- function (data, value = "l30", mincor = .8)  {
    bullet <- NULL
    b12 <- unique(data$bullet)
    if (length(b12) != 2) 
        stop("Two surfaces should be compared\n\n")
    data$val <- data.frame(data)[, value]

    subLOFx1 <- subset(data, bullet == b12[1])
    subLOFx2 <- subset(data, bullet == b12[2])
    subLOFx1$y <- subLOFx1$y - min(subLOFx1$y)
    subLOFx2$y <- subLOFx2$y - min(subLOFx2$y)

    whichmin <- which.min(c(length(subLOFx1$y), length(subLOFx2$y)))
    shorter <- list(subLOFx1$val, subLOFx2$val)[[whichmin]]
    longer <- list(subLOFx1$val, subLOFx2$val)[[3 - whichmin]]
    
    longer_na <- c(rep(0, length(shorter)), longer, rep(0, (length(shorter))))
    
    mycors <- NULL
    for (i in 1:(length(longer_na) - length(shorter))) {
        longersub <- longer_na[i:(i + length(shorter) - 1)]
        
        corval <- cor(shorter, longersub, use = "pairwise.complete.obs")

        mycors <- c(mycors, corval)
    }
    
    lag <- which.max(mycors) - length(shorter)
    if (max(mycors, na.rm = TRUE) < mincor) lag <- 0
    
    incr <- min(diff(sort(unique(subLOFx1$y))))
    
    mydat <- if (whichmin == 1) subLOFx1 else subLOFx2
    mydat2 <- if (whichmin == 1) subLOFx2 else subLOFx1

    if (lag < 0) {
        mydat2$y <- mydat2$y + lag * incr
    } else {
        mydat$y <- mydat$y + lag * incr
    }

    bullets <- rbind(data.frame(mydat), data.frame(mydat2))
    list(ccf = max(mycors), lag = lag * incr, bullets = bullets)
}

lapply(b3s, function(b3) {
    cat(b3, "\n")
    test <- strsplit(b3, "___")
    
    mycc1 <- ccs[ccs$path == test[[1]][1],"cc"]
    b1 <- processBullets(read.x3p(test[[1]][1]), name = "b1", x = mycc1)
    
    mycc2 <- ccs[ccs$path == test[[1]][2],"cc"]
    #mycc2 <- mycc1
    b2 <- processBullets(read.x3p(test[[1]][2]), name = "b2", x = mycc2)
    
    mybul <- rbind(b1, b2)
    mysmooth <- bulletSmooth(mybul)
    
    print(
        qplot(y, l30, data = mysmooth, geom = "line", colour = bullet)
    )
    
    result <- readline("Press Enter to Continue")
    
    result <- bulletAlign_test(mysmooth, value = "l30")
    
    print(
        qplot(y, l30, data = result[[3]], geom = "line", colour = bullet)
    )
    
    result <- readline("Press Enter to Continue or q to quit")
    if (result == "q") break;
})

