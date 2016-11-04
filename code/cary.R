library(x3pr)
library(x3prplus)
library(ggplot2)
library(dplyr)
library(randomForest)

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
cary1_processed <- processBullets(bullet = cary1, name = cary_paths[1], x = myx, grooves = cary1_groove$groove)
cary1_smoothed <- bulletSmooth(cary1_processed)

other_carys <- lapply(cary_paths[-1], function(x) {
    result <- read.x3pplus(x)
    result[[3]] <- x
    names(result)[3] <- "path"
    
    return(result)
})

carys_processed <- lapply(other_carys, function(bul) {
    cat("Computing processed bullet", basename(bul$path), "\n")
    
    #myx <- bulletCheckCrossCut(path = bul$path, bullet = bul)
    #if (is.na(myx)) myx <- 200
    myx <- 200
    
    cary_cross <- get_crosscut(path = bul$path, bullet = bul)
    cary_groove <- get_grooves(cary_cross, smoothfactor = 35)

    processBullets(bullet = bul, name = bul$path, x = myx, grooves = cary_groove$groove)
})
names(carys_processed) <- cary_paths[-1]
cary_smoothed <- carys_processed %>% bind_rows %>% bulletSmooth

if (!file.exists("cary-data")) dir.create("cary-data")
reslist <- lapply(other_carys, function(x) {
    cat("Processing cary1 vs", basename(x$path), "\n")
    
    br2 <- filter(cary_smoothed, bullet == x$path)
    bulletGetMaxCMS(cary1_smoothed, br2, column = "l30", span = 25)
})

ccf <- plyr::ldply(reslist, function(res) {
    if (is.null(res)) return(NULL)
    lofX <- res$bullets
    b12 <- unique(lofX$bullet)
    
    subLOFx1 <- subset(lofX, bullet==b12[1])
    subLOFx2 <- subset(lofX, bullet==b12[2]) 
    
    ys <- intersect(subLOFx1$y, subLOFx2$y)
    idx1 <- which(subLOFx1$y %in% ys)
    idx2 <- which(subLOFx2$y %in% ys)
    distr.dist <- mean((subLOFx1$val[idx1] - subLOFx2$val[idx2])^2, na.rm=TRUE)
    distr.sd <- sd(subLOFx1$val, na.rm=TRUE) + sd(subLOFx2$val, na.rm=TRUE)
    km <- which(res$lines$match)
    knm <- which(!res$lines$match)
    if (length(km) == 0) km <- c(length(knm)+1,0)
    if (length(knm) == 0) knm <- c(length(km)+1,0)
    #browser()    
    # feature extraction
    signature.length <- min(nrow(subLOFx1), nrow(subLOFx2))
    
    data.frame(ccf=res$ccf, lag=res$lag, 
               D=distr.dist, 
               sd.D = distr.sd,
               b1=b12[1], b2=b12[2], x1 = subLOFx1$x[1], x2 = subLOFx2$x[1],
               signature.length = signature.length,
               #num.matches = sum(res$lines$match),
               matches.per.y = sum(res$lines$match) / signature.length,
               #num.mismatches = sum(!res$lines$match), 
               mismatches.per.y = sum(!res$lines$match) / signature.length,
               #cms = res$maxCMS,
               cms.per.y = res$maxCMS / signature.length,
               #cms2 = x3prplus::maxCMS(subset(res$lines, type==1 | is.na(type))$match),
               cms2.per.y = x3prplus::maxCMS(subset(res$lines, type==1 | is.na(type))$match) / signature.length,
               #non_cms = x3prplus::maxCMS(!res$lines$match),
               non_cms.per.y = x3prplus::maxCMS(!res$lines$match) / signature.length,
               #left_cms = max(knm[1] - km[1], 0),
               left_cms.per.y = max(knm[1] - km[1], 0) / signature.length,
               #right_cms = max(km[length(km)] - knm[length(knm)],0),
               right_cms.per.y = max(km[length(km)] - knm[length(knm)],0) / signature.length,
               #left_noncms = max(km[1] - knm[1], 0),
               left_noncms.per.y = max(km[1] - knm[1], 0) / signature.length,
               #right_noncms = max(knm[length(knm)]-km[length(km)],0),
               right_noncms.per.y = max(knm[length(knm)]-km[length(km)],0) / signature.length,
               #sumpeaks = sum(abs(res$lines$heights[res$lines$match]))
               sumpeaks.per.y = sum(abs(res$lines$heights[res$lines$match])) / signature.length
    )
})
write.csv(ccf, file = "cary-stats.csv", row.names = FALSE)
myprobs <- predict(rtrees, ccf, type = "prob")

iteration <- as.numeric(gsub("../images/Cary Persistence/bullets/CWBLT([0-9]+)-1.x3p", "\\1", cary_paths))
qplot(iteration[-1], myprobs[,2], geom = "point") + 
    geom_smooth() +
    theme_bw() + 
    scale_x_continuous(breaks = c(0, 75, 150, 250, 500, 750, 1000, 1500, 2000)) +
    ylim(c(0, 1)) +
    xlab("Cary Persistence Iteration") +
    ylab("Predicted Probability of Match ") +
    geom_hline(yintercept = 0.5) +
    ggtitle("Predicted Prob of Match (Cary 1 vs Cary 2-2000)")

test <- rbind(cary1_smoothed, cary_smoothed) %>%
    mutate(iteration = as.numeric(gsub("../images/Cary Persistence/bullets/CWBLT([0-9]+)-1.x3p", "\\1", bullet)))

mydat <- filter(test, iteration %in% c(1, 10, 50, 100, 250, 500, 1000, 2000)) %>%
    group_by(iteration) %>%
    mutate(y = y - min(y))
qplot(y, l30, data = mydat, geom = "line") +
    facet_wrap(~iteration, ncol = 1) +
    theme_bw() +
    ggtitle("Cary Signatures at Specified Iterations")

