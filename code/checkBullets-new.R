library(x3pr)
library(x3prplus)
library(dplyr)
library(ggplot2)
library(zoo)

knowndatadir <- "~/Downloads/Hamby Set 44/known/"
knowns <- file.path(knowndatadir, dir(knowndatadir, pattern="x3p"))

unknowndatadir <- "~/Downloads/Hamby Set 44/unknown/"
unknowns <- file.path(unknowndatadir, dir(unknowndatadir))

if (!file.exists("csvs/crosscuts.csv")) {
crosscuts_known <- sapply(knowns, function(x) {
  cat(x)
  crosscut <- bulletCheckCrossCut(x, x = seq(100, 750, by=25), transpose = TRUE)
  cat(crosscut)
  cat("\n")
  crosscut
})
crosscuts_unknown <- sapply(unknowns, function(x) {
    cat(x)
    crosscut <- bulletCheckCrossCut(x, x = seq(100, 750, by=25), transpose = TRUE)
    cat(crosscut)
    cat("\n")
    crosscut
})
crosscuts <- c(crosscuts_known, crosscuts_unknown)

dframe <- data.frame(
  path = c(knowns, unknowns),
  cc = crosscuts
)


write.csv(dframe, file="csvs/crosscuts-25.csv", row.names=FALSE)
} 



ccs <- read.csv("data/crosscuts.csv")


bullets <- lapply(c(knowns, unknowns), function(path) {
  lof <- processBullets(paths = path, x = ccs$cc[which(ccs$path==path)], check=FALSE)
  peaks <- get_peaks(lof)

  list(path=path, bullet=lof, peaks=peaks, lines=peaks$lines)
})
