ccfs1 <- read.csv("data-25-25/bullet-stats.csv")
ccfs2 <- read.csv("data-set44-25-25/bullet-stats-set44.csv")

all_ccfs <- rbind(ccfs1, ccfs2)
all_ccfs <- all_ccfs[!duplicated(t(apply(all_ccfs[,c("b2", "b1")], 1, sort))),]


training_inds <- sample(1:nrow(all_ccfs), size = round(.8 * nrow(all_ccfs)))
training_ccfs <- all_ccfs[training_inds,]
test_ccfs <- all_ccfs[-training_inds,]

library(rpart)
library(rpart.plot)
includes <- setdiff(names(CCFs), c("b1", "b2", "data", "resID", "id.x", "id.y", "pred", "span", "forest"))
rp1 <- rpart(match~., training_ccfs[,includes])  # doesn't include cms at all !!!!
prp(rp1, extra = 101)

includes2 <- setdiff(includes, c("left_cms.per.y", "right_cms.per.y", "left_noncms.per.y", "right_noncms.per.y", "cms2.per.y"))
library(randomForest)
rtrees <- randomForest(factor(match)~., data=training_ccfs[,includes2], ntree=300)
training_ccfs$forest <- predict(rtrees, type="prob")[,2]
imp <- data.frame(importance(rtrees))

xtabs(~(forest>0.5)+match+span, data=training_ccfs)

test_ccfs$forest <- predict(rtrees, test_ccfs, type = "prob")[,2]
xtabs(~(forest>0.5)+match+span, data=test_ccfs)
