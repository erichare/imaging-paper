library(ggplot2)
library(x3pr)
library(x3prplus)
library(dtw)

b1 <- read.x3p("GitHub/imaging-paper/app/images/Hamby252_3DX3P1of2/Br1 Bullet 1-1.x3p")
b2 <- read.x3p("GitHub/imaging-paper/app/degraded_images/Hamby252_3DX3P1of2/Br1_Bullet_1-1.x3p")

bb1 <- fortify_x3p(b1)
bb2 <- fortify_x3p(b2)

b1sub <- subset(bb1, x == 100)
b2sub <- subset(bb2, x == 100)

regular <- b1sub$value
degraded <- b2sub$value

regular_nona <- regular
regular_nona[is.na(regular_nona)] <- 0
degraded_nona <- degraded
degraded_nona[is.na(degraded_nona)] <- 0

qplot(y, value, data = b1sub, colour = I("red"), geom = "line") + theme_bw() +
    geom_line(data = b2sub, aes(x = y, y = value), colour = I("blue"))

alignment <- dtw(regular_nona, degraded_nona)
min.ind <- tail(which(alignment$index2 == 1), n = 1)
incr <- diff(b1sub$y[1:2])
b2sub$y <- b2sub$y + (incr * min.ind)

qplot(y, value, data = b1sub, colour = I("red"), geom = "line") + theme_bw() +
    geom_line(data = b2sub, aes(x = y, y = value), colour = I("blue"))
