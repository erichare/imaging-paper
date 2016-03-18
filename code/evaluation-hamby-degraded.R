span <- 25
dataStr <- sprintf("data-degraded-%d-25", span)
datas <- file.path(dataStr, dir(dataStr, pattern="RData"))
datas <- datas[grep(paste0(dataStr,"/u.*"), datas)]

CCFs <- plyr::ldply(datas, function(x) {
  load(x)
#  cmsdist <- sapply(reslist, function(x) x$maxCMS)
  ccf <- plyr::ldply(reslist, function(res) {
    lofX <- res$bullets
    b12 <- unique(lofX$bullet)
    
    subLOFx1 <- subset(lofX, bullet==b12[1])
    subLOFx2 <- subset(lofX, bullet==b12[2]) 
    
    subLOFx1$y <- subLOFx1$y - min(subLOFx1$y)
    subLOFx2$y <- subLOFx2$y - min(subLOFx2$y)

    ccf <- ccf(subLOFx1$val, subLOFx2$val, plot = FALSE, lag.max=200, na.action = na.omit)
    lag <- ccf$lag[which.max(ccf$acf)]
    incr <- min(diff(sort(unique(subLOFx1$y))))
    
    subLOFx1$y <- subLOFx1$y -  lag * incr # amount of shifting should just be lag * y.inc
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
    data.frame(ccf=max(ccf$acf), lag=which.max(ccf$acf), 
               D=distr.dist, 
               sd.D = distr.sd,
               b1=b12[1], b2=b12[2], x1 = subLOFx1$x[1], x2 = subLOFx2$x[1],
               #num.matches = sum(res$lines$match), 
               matches.per.y = sum(res$lines$match) / nrow(res$bullets),
               #num.mismatches = sum(!res$lines$match), 
               mismatches.per.y = sum(!res$lines$match) / nrow(res$bullets),
               #cms = res$maxCMS,
               cms.per.y = res$maxCMS / nrow(res$bullets),
               #cms2 = x3prplus::maxCMS(subset(res$lines, type==1 | is.na(type))$match),
               cms2.per.y = x3prplus::maxCMS(subset(res$lines, type==1 | is.na(type))$match) / nrow(res$bullets),
               #non_cms = x3prplus::maxCMS(!res$lines$match),
               non_cms.per.y = x3prplus::maxCMS(!res$lines$match) / nrow(res$bullets),
               #left_cms = max(knm[1] - km[1], 0),
               left_cms.per.y = max(knm[1] - km[1], 0) / nrow(res$bullets),
               #right_cms = max(km[length(km)] - knm[length(knm)],0),
               right_cms.per.y = max(km[length(km)] - knm[length(knm)],0) / nrow(res$bullets),
               #left_noncms = max(km[1] - knm[1], 0),
               left_noncms.per.y = max(km[1] - knm[1], 0) / nrow(res$bullets),
               #right_noncms = max(knm[length(knm)]-km[length(km)],0),
               right_noncms.per.y = max(knm[length(knm)]-km[length(km)],0) / nrow(res$bullets),
               #sumpeaks = sum(abs(res$lines$heights[res$lines$match]))
               sumpeaks.per.y = sum(abs(res$lines$heights[res$lines$match])) / nrow(res$bullets),
               signature_length = nrow(res$bullets)
               )
  })
#  ccf$cms <- cmsdist
  ccf$data <- x
  ccf  
})

CCFs$resID <- rep(1:120, length=nrow(CCFs))
CCFs <- CCFs[order(as.character(CCFs$b2)),]
CCFs$b2 <- factor(as.character(CCFs$b2))

splits <- strsplit(as.character(CCFs$b1), split="/")
CCFs$b1 <- sapply(splits, function(x) x[length(x)])
CCFs$b1 <- gsub(".x3p","", CCFs$b1)
  
splits <- strsplit(as.character(CCFs$b2), split="/")
CCFs$b2 <- sapply(splits, function(x) x[length(x)])
CCFs$b2 <- gsub(".x3p","", CCFs$b2)


matches <- read.csv("csvs/matches.csv", header=FALSE, stringsAsFactors = FALSE)
matches$V3 <- paste("Ukn Bullet",matches$V3)
matches$V4 <- paste("Ukn Bullet",matches$V4)
matches$V5 <- paste("Ukn Bullet",matches$V5)
matches$id <- 1:nrow(matches)

library(reshape2)
mm <- melt(matches, id.var="id")
mm <- subset(mm, value != "Ukn Bullet ")

# CCFs <- merge(CCFs, mm[,c("id","value")], by.x="b1", by.y="value")
# CCFs <- merge(CCFs, mm[,c("id","value")], by.x="b2", by.y="value")
# CCFs$match <- CCFs$id.x == CCFs$id.y
# CCFs$span <- span


library(rpart)
library(rpart.plot)
includes <- setdiff(names(CCFs), c("b1", "b2", "data", "resID", "id.x", "id.y", "pred", "span", "forest"))
rp1 <- rpart(match~., CCFs[,includes])  # doesn't include cms at all !!!!
prp(rp1, extra = 101)
CCFs$pred <- predict(rp1)

includes2 <- setdiff(includes, c("left_cms.per.y", "right_cms.per.y", "left_noncms.per.y", "right_noncms.per.y", "cms2.per.y"))
library(randomForest)
set.seed(20160105)
rtrees <- randomForest(factor(match)~., data=CCFs[,includes2], ntree=300)
CCFs$forest <- predict(rtrees, type="prob")[,2]
imp <- data.frame(importance(rtrees))

includes3 <- c(setdiff(includes2, "cms.per.y"), "cms2.per.y")
set.seed(20160105)
rtrees1b <- randomForest(factor(match)~., data=CCFs[,includes3], ntree=300)
imp1b <- data.frame(importance(rtrees1b))



write.csv(CCFs, file=file.path(dataStr, "bullet-stats-degraded.csv"), row.names=FALSE)


##################################################


library(ggplot2)
CCFs <- read.csv(file.path(dataStr, "bullet-stats-degraded.csv"))

means <- CCFs %>% group_by(match) %>% summarize(
  meanx = mean(ccf), 
  sdx = sd(ccf),
  meany = mean(matches.per.y),
  sdy = sd(matches.per.y))

#####################
# throw in all three evaluations into the mix:

bstats <- NULL
for (i in c(25)) {
  dataStr <- sprintf("data-degraded-%d-25", i)
  temp <- read.csv(file.path(dataStr, "bullet-stats-degraded.csv"))
  includes <- setdiff(names(temp), c("b1", "b2", "data", "resID", "id.x", "id.y"))
  temp$diffx <- with(temp, abs(x1-x2))
  temp$perc_matches <- with(temp, matches.per.y/(matches.per.y+mismatches.per.y))
  rp <- rpart(match~., data=temp[,includes])
  
  prp(rp, extra = 101)
  #ch <- scan()
  temp$pred <- predict(rp)
  temp$span <- i
  temp$bullet <- NULL
  temp$crosscutdist <- NULL
  bstats <- rbind(bstats, temp)
}



xtabs(~(pred>0.5)+match+span, data=bstats)

bstats$bullet <- gsub("-[0-9]$", "", bstats$b2)
bullets <- bstats %>% group_by(bullet, span) %>% summarize(
  n = n(),
  pred = sum(pred > 0.5)
)

qplot(data=bullets, pred, reorder(bullet, pred/n),  colour=factor(span), shape = factor(span)) + 
  theme_bw()  + ylab("") + 
  scale_x_continuous("Number of correctly predicted land-to-land matches", 
                     breaks = 3*0:4, limits=c(0,12)) 

write.csv(bstats, "csvs/bullet-stats-degraded.csv", row.names=TRUE)


