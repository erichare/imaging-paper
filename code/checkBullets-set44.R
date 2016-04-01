library(x3pr)
library(x3prplus)
library(dplyr)
library(ggplot2)
library(zoo)

knowndatadir <- "~/Downloads/Hamby Set 44/known/"
knowns <- file.path(knowndatadir, dir(knowndatadir, pattern="x3p"))

unknowndatadir <- "~/Downloads/Hamby Set 44/unknown/"
unknowns <- file.path(unknowndatadir, dir(unknowndatadir))

if (!file.exists("csvs/crosscuts-25-set44.csv")) {
crosscuts <- sapply(c(knowns, unknowns), function(x) {
    cat(x)
    crosscut <- bulletCheckCrossCut(x, xlimits = seq(100, 500, by = 25), transpose = TRUE)
    cat(crosscut)
    cat("\n")
    crosscut
})
crosscuts[is.na(crosscuts)] <- 150

dframe <- data.frame(
  path = c(knowns, unknowns),
  cc = crosscuts
)


write.csv(dframe, file="csvs/crosscuts-25-set44.csv", row.names=FALSE)
}
