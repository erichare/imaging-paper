library(x3pr)
library(x3prplus)
library(dplyr)
library(ggplot2)

knowndatadir <- "app/images/Hamby252_3DX3P1of2/"
knowns <- file.path(knowndatadir, dir(knowndatadir, pattern="x3p"))

crosscuts1 <- sapply(knowns, function(x) {
  cat(x)
  crosscut <- bulletCheckCrossCut(x, x = seq(100, 500, by=12.5))
  cat(crosscut)
  cat("\n")
  crosscut
})



unknowndatadir <- "app/images/Hamby252_3DX3P2of2/"
unknowns <- file.path(unknowndatadir, dir(unknowndatadir))

crosscuts2 <- sapply(unknowns, function(x) {
  cat(x)
  crosscut <- bulletCheckCrossCut(x, x = seq(100, 500, by=12.5))
  cat(crosscut)
  cat("\n")
  crosscut
})

dframe <- data.frame(
  path = c(knowns, unknowns),
  cc = c(crosscuts1, crosscuts2)
)


write.csv(dframe, file="crosscuts.csv", row.names=FALSE)
ccs <- read.csv("data/crosscuts.csv")

knowndatadir <- "app/images/Hamby252_3DX3P1of2/"
knowns <- file.path(knowndatadir, dir(knowndatadir, pattern="x3p"))

bullets <- lapply(c(knowns, unknowns), function(path) {
  lof <- processBullets(paths = path, x = ccs$cc[which(ccs$path==path)], check=FALSE)
  peaks <- get_peaks(lof)
  peaks$extrema <- c(peaks$peaks, peaks$valleys)
  peaks$type <- c(rep(1, length(peaks$peaks)), rep(-1, length(peaks$valleys)))
  idx <- order(peaks$extrema)
  peaks$extrema <- peaks$extrema[idx]
  peaks$type <- peaks$type[idx]
  diffs <- diff(peaks$extrema)
  lines <- data.frame(xmin = peaks$extrema-c(diffs[1],diffs)/3,
                      xmax=peaks$extrema+c(diffs,diffs[length(diffs)])/3, 
                      type=peaks$type)
  list(path=path, bullet=lof, peaks=peaks, lines=lines)
})