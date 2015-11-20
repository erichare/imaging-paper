library(x3pr)
library(x3prplus)


processBullets <- function(paths, x = 100) {
  br111 <- read.x3p(paths[1])
  crosscuts <- unique(fortify_x3p(br111)$x)
  crosscuts <- crosscuts[crosscuts >= min(x)]
  crosscuts <- crosscuts[crosscuts <= max(x)]
  
  LOF <- lapply(paths, function(path) {
    list_of_fits <- lapply(crosscuts, function(x) {
      br111 <- get_bullet(path, x = x)
      br111.groove <- get_grooves(br111)
      br111.groove$plot
      fit_loess(br111, br111.groove)
    })
    lof <- lapply(list_of_fits, function(x) x$resid$data) %>% bind_rows
    lof$path <- path
    path <- gsub("app.*//", "", as.character(path))
    lof$bullet <- gsub(".x3p", "", path)
    
    lof
  })
  LOF %>% bind_rows()
}

smoothloess <- function(x, y, span, sub = 2) {
  dat <- data.frame(x, y)
  indx <- sub *(1: (nrow(dat) %/% sub))
  subdat <- dat[indx, ]
  lwp <- with(subdat, loess(y~x,span=span))
  predict(lwp, newdata = dat)
}

datadir <- "app/images/Hamby252_3DX3P1of2/"
images <- file.path(datadir, dir(datadir))

lof <- processBullets(paths = images[c(5,7)], x = 100)

subLOFx1 <- subset(lof, bullet=="Br1 Bullet 1-5")
subLOFx2 <- subset(lof, bullet=="Br1 Bullet 2-1")
subLOFx1$y <- subLOFx1$y + 19*1.5625 # working now!!!
lof <- rbind(data.frame(subLOFx1), data.frame(subLOFx2))



# couple of side by side comparisons
qplot(bullet, y, fill = resid, data=lof, geom="tile") + 
  scale_fill_gradient2() + theme_bw()

qplot(y, resid, group=bullet, colour=bullet, data=lof, geom="line") + 
  scale_colour_brewer(palette="Set1") + theme_bw() +
  theme(legend.position="bottom")


lof <- lof %>% group_by(bullet) %>% mutate(
  l30 = smoothloess(y, resid, span = 0.03)
)
lof$r05 <- 0.75* sign(lof$l30) * as.numeric(abs(lof$l30) > .75)
lof$type <- factor(lof$r05)
levels(lof$type) <- c("groove", NA, "peak")

qplot(data=lof, x=y, y=l30, colour=type, geom="line", group=bullet) + 
  facet_grid(bullet~.) +
  geom_point(aes(y=r05))

qplot(data=lof, x=y, y=bullet, fill=type, geom="tile") +
  scale_fill_brewer(palette="Set1")


