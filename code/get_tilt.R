library(ggplot2)
library(MASS)

grooves <- read.csv("csvs/grooves.csv")

br111.groove <- subset(grooves, bullet == "images/Hamby (2009) Barrel/bullets/Br1 Bullet 1-3.x3p")
qplot(x, groove_right, data = br111.groove)
qplot(groove_right, x, data = br111.groove) +
    geom_point(aes(x = groove_left, y = x))

rlm(groove_right ~ x, data = br111.groove)
rlm(groove_left ~ x, data = br111.groove)

which(br111.groove$manual)
