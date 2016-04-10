library(x3pr)
library(x3prplus)
library(dplyr)
library(ggplot2)
library(zoo)

knowndatadir <- "app/images/Hamby252_3DX3P1of2/"
knowns <- file.path(knowndatadir, dir(knowndatadir, pattern="x3p"))

unknowndatadir <- "app/images/Hamby252_3DX3P2of2/"
unknowns <- file.path(unknowndatadir, dir(unknowndatadir))

if (!file.exists("csvs/crosscuts-25.csv")) {
crosscuts <- sapply(c(knowns, unknowns), function(x) {
  cat(x)
  crosscut <- bulletCheckCrossCut(x, xlimits = seq(100, 500, by = 25))
  cat(crosscut)
  cat("\n")
  crosscut
})
crosscuts[is.na(crosscuts)] <- 150

dframe <- data.frame(
  path = c(knowns, unknowns),
  cc = crosscuts
)


write.csv(dframe, file="csvs/crosscuts-25.csv", row.names=FALSE)
}
