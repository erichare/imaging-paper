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
    transpose <- (length(grep(" ", x)) == 0)
  cat(x, "- transposing:", transpose)
  crosscut <- bulletCheckCrossCut(x, xlimits = c(20, 400), distance = 20, transpose = transpose)
  cat(crosscut)
  cat("\n")
  crosscut
})
crosscuts[is.na(crosscuts)] <- 100

dframe <- data.frame(
  path = c(knowns, unknowns),
  cc = crosscuts
)


write.csv(dframe, file="csvs/crosscuts-25.csv", row.names=FALSE)
}
