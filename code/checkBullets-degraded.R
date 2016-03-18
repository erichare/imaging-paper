library(x3pr)
library(x3prplus)
library(dplyr)
library(ggplot2)
library(zoo)

knowndatadir <- "app/images/Hamby252_3DX3P1of2/"
knowns <- file.path(knowndatadir, dir(knowndatadir, pattern="x3p"))

unknowndatadir <- "app/degraded_images/Hamby252_3DX3P1of2/"
unknowns <- file.path(unknowndatadir, dir(unknowndatadir))

if (!file.exists("csvs/crosscuts-25-degraded.csv")) {
crosscuts_known <- sapply(knowns, function(x) {
  cat(x)
  crosscut <- bulletCheckCrossCut(x, x = seq(100, 750, by=25))
  cat(crosscut)
  cat("\n")
  crosscut
})
crosscuts_unknown <- sapply(unknowns, function(x) {
    cat(x)
    crosscut <- bulletCheckCrossCut(x, xlimits = seq(100, 750, by=25), span = 0.15)
    cat(crosscut)
    cat("\n")
    crosscut
})
crosscuts <- c(crosscuts_known, crosscuts_unknown)

dframe <- data.frame(
  path = c(knowns, unknowns),
  cc = crosscuts
)


write.csv(dframe, file="csvs/crosscuts-25-degraded.csv", row.names=FALSE)
}
