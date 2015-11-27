setwd("data")
datas <- dir()

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
    
    data.frame(ccf=max(ccf$acf), lag=which.max(ccf$acf), distr.dist=distr.dist, b1=b12[1], b2=b12[2])
  })
  ccf$cms <- cmsdist
  ccf$data <- x
  ccf  
})

CCFs$resID <- rep(1:120, length=nrow(CCFs))

idx <- which(CCFs$ccf > 0.6 & CCFs$distr.dist < 1.25)
for ( i in idx) {
  load(CCFs$data[i])
  res <- reslist[[CCFs$resID[i]]]  

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



qplot(maxCMS)
idx <- which(maxCMS > 9) # definitely matches
idx <- which(maxCMS == 8) # not a match
idx <- which(maxCMS == 7) # one is a match
idx <- which(maxCMS == 6) # two are a match



for (i in idx) {
load(datas[i])

cmsdist <- sapply(reslist, function(x) x$maxCMS)


qplot(cmsdist, geom="bar") + theme_bw() + xlab("Number of CMS")
res <- reslist[[which.max(cmsdist)]]  
ch <- scan()
#res <- reslist[[which.max(cmsdist[-which.max(cmsdist)])]]  # number 2

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