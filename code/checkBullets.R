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
