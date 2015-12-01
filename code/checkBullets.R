library(x3pr)
library(x3prplus)
library(dplyr)
library(ggplot2)

knowndatadir <- "app/images/Hamby252_3DX3P1of2/"
knowns <- file.path(knowndatadir, dir(knowndatadir, pattern="x3p"))


dframe <- data.frame(
  path = knowns,
  cc = crosscuts
)


unknowndatadir <- "app/images/Hamby252_3DX3P2of2/"
unknowns <- file.path(unknowndatadir, dir(unknowndatadir))

crosscuts <- sapply(unknowns, function(x) {
  crosscut <- bulletCheckCrossCut(x, x = seq(100, 500, by=12.5))
})

dframe2 <- data.frame(
  path = unknowns,
  cc = crosscuts
)


write.csv(rbind(dframe, dframe2), file="crosscuts.csv", row.names=FALSE)
