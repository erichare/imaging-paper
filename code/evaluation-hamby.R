
datas <- file.path("data", dir("data", pattern="RData"))
datas <- datas[grep("data/u.*", datas)]

maxCMS <- sapply(datas, function(x) {
  load(x)
  cmsdist <- sapply(reslist, function(x) x$maxCMS)
  max(cmsdist)
})


CCFs <- plyr::ldply(datas, function(x) {
  load(x)
  cmsdist <- sapply(reslist, function(x) x$maxCMS)
  ccf <- plyr::ldply(reslist, function(res) {
    lofX <- res$bullets
    b12 <- unique(lofX$bullet)
    
    subLOFx1 <- subset(lofX, bullet==b12[1])
    subLOFx2 <- subset(lofX, bullet==b12[2]) 
    
    subLOFx1$y <- subLOFx1$y - min(subLOFx1$y)
    subLOFx2$y <- subLOFx2$y - min(subLOFx2$y)
 #     browser()
    ccf <- ccf(subLOFx1$val, subLOFx2$val, plot = FALSE, lag.max=200, na.action = na.omit)
    lag <- ccf$lag[which.max(ccf$acf)]
    incr <- min(diff(sort(unique(subLOFx1$y))))
    
    subLOFx1$y <- subLOFx1$y -  lag * incr # amount of shifting should just be lag * y.inc
    ys <- intersect(subLOFx1$y, subLOFx2$y)
    idx1 <- which(subLOFx1$y %in% ys)
    idx2 <- which(subLOFx2$y %in% ys)
    distr.dist <- mean((subLOFx1$val[idx1] - subLOFx2$val[idx2])^2, na.rm=T)
    
    # feature extraction
    data.frame(ccf=max(ccf$acf), lag=which.max(ccf$acf), distr.dist=distr.dist, 
               b1=b12[1], b2=b12[2], x1 = subLOFx1$x[1], x2 = subLOFx2$x[1],
               num.matches = sum(res$lines$match), 
               num.mismatches = sum(!res$lines$match), 
               non_cms = x3prplus::maxCMS(res$lines$match))
  })
  ccf$cms <- cmsdist
  ccf$data <- x
  ccf  
})

CCFs$resID <- rep(1:120, length=nrow(CCFs))
CCFs <- CCFs[order(as.character(CCFs$b2)),]
CCFs$b2 <- factor(as.character(CCFs$b2))

CCFs$match <- NA
idx <- which(CCFs$cms >= 9) # all are matches, visually confirmed
CCFs$match[idx] <- TRUE
write.csv(CCFs, file="bullet-stats.csv", row.names=FALSE)


for ( i in idx) {
  
  load(CCFs$data[i])
  res <- reslist[[CCFs$resID[i]]]  

  ch <- scan()
  print(ggplot() +
          theme_bw() + 
          geom_rect(aes(xmin=miny, xmax=maxy), ymin=-6, ymax=5, fill="grey90", data=res$lines) +
          geom_line(aes(x = y, y = l30, colour = bullet),  data = res$bullets) +
          geom_hline(yintercept = res$threshold) +
          geom_hline(yintercept = - res$threshold) +
          scale_colour_brewer(palette="Set1") +
          theme(legend.position = c(1,1), legend.justification=c(1,1)) + 
          ylim(c(-6,6)) +
          geom_text(aes(x = meany), y= -5.5, label= "x", data = subset(res$lines, !match)) +
          geom_text(aes(x = meany), y= -5.5, label= "o", data = subset(res$lines, match)))
  
}


matches <- read.csv("csvs/matches.csv", header=FALSE, stringsAsFactors = FALSE)
matches$V3 <- paste("Ukn Bullet",matches$V3)
matches$V4 <- paste("Ukn Bullet",matches$V4)
matches$V5 <- paste("Ukn Bullet",matches$V5)
matches$id <- 1:nrow(matches)

library(reshape2)
mm <- melt(matches, id.var="id")
mm <- subset(mm, value != "Ukn Bullet ")

CCFs <- merge(CCFs, mm[,c("id","value")], by.x="b1", by.y="value")
CCFs <- merge(CCFs, mm[,c("id","value")], by.x="b2", by.y="value")
CCFs$match <- CCFs$id.x == CCFs$id.y

write.csv(CCFs, file="data/bullet-stats.csv", row.names=FALSE)
