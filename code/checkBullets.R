library(x3pr)
library(x3prplus)
library(dplyr)
library(ggplot2)
library(zoo)

datadir <- "images/Hamby (2009) Barrel/bullets"
all_data <- file.path(datadir, dir(datadir))
knowns <- all_data[grep("[B]r[0-9].*", all_data)]
unknowns <-  all_data[grep("Ukn*", all_data)]

if (!file.exists("csvs/crosscuts.csv")) {
crosscuts <- sapply(c(knowns, unknowns), function(x) {
    transpose <- (length(grep(" ", x)) == 0)
  cat(x, "- transposing:", transpose)
  crosscut <- bulletCheckCrossCut(x, xlimits = c(120, 400), distance = 20, transpose = transpose)
  cat(crosscut)
  cat("\n")
  crosscut
})

dframe <- data.frame(
  path = c(knowns, unknowns),
  cc = crosscuts
)

write.csv(dframe, file="csvs/crosscuts.csv", row.names=FALSE)
}
