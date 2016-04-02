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
crosscuts <- sapply(c(knowns, unknowns), function(x) {
    cat(x)
    crosscut <- bulletCheckCrossCut(x, x = seq(100, 500, by=25), span = 0.15, minccf = .99)
    cat(crosscut)
    cat("\n")
    crosscut
})

dframe <- data.frame(
  path = c(knowns, unknowns),
  cc = crosscuts
)


write.csv(dframe, file="csvs/crosscuts-25-degraded.csv", row.names=FALSE)
}
