
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
               non_cms = x3prplus::maxCMS(!res$lines$match))
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

idx <- which(CCFs$match & CCFs$cms <= 6)

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

# diagnostics

library(ggplot2)
CCFs <- read.csv("data-35/bullet-stats.csv")

qplot(factor(cms), data=CCFs)
ggplot(data=CCFs) + geom_bar(aes(x=factor(cms), fill=match), position="fill")
ggplot(data=CCFs) + geom_jitter(aes(x=factor(cms), y=distr.dist, colour=match))
ggplot(data=CCFs) + geom_jitter(aes(x=factor(cms), y=ccf, colour=match)) + facet_wrap(~match)
ggplot(data=CCFs) + geom_bar(aes(x=factor(non_cms), fill=match), position="fill")

library(rpart)
includes <- setdiff(names(CCFs), c("b1", "b2", "data", "resID", "id.x", "id.y"))
rp1 <- rpart(match~., CCFs[,includes])  # doesn't include cms at all !!!!
plot(rp1)
text(rp1) 

qplot(ccf, num.matches, geom="jitter", data=CCFs, colour=match, alpha=0.1) + facet_wrap(~match)

CCFs$pred35 <- predict(rp1)
xtabs(~pred35+match, data=CCFs)

# the ccf/num.matches distribution of non-matches looks like a normal distribution - 
# let's find a mean and compute (a standardized) distance from that

means <- CCFs %>% group_by(match) %>% summarize(
  meanx = mean(ccf), 
  sdx = sd(ccf),
  meany = mean(num.matches),
  sdy = sd(num.matches))

CCFs$dist <- sqrt(with(CCFs, (num.matches-means$meany[1])^2/means$sdy[1]^2 + (ccf-means$meanx[1])^2/means$sdx[1]^2 ))
subCCFs <- subset(CCFs, match==FALSE)
qplot(ccf, num.matches, geom="jitter", data=subCCFs, colour=dist) 

includes <- setdiff(names(CCFs), c("b1", "b2", "data", "resID", "id.x", "id.y", "pred35"))
rp2 <- rpart(match~., CCFs[,includes])  
plot(rp2)
text(rp2) 
CCFs$pred352 <- predict(rp2)
xtabs(~pred352+match, data=CCFs)

# not actually an improvement - now there's two false positives in the mix

includes <- setdiff(names(CCFs), c("b1", "b2", "data", "resID", "id.x", "id.y", "pred35"))
rp3 <- rpart(match~., CCFs[,includes], parms=list(prior=c(0.5,0.5)) )
plot(rp3)
text(rp3) 
CCFs$pred353 <- predict(rp3)
xtabs(~pred353+match, data=CCFs)
