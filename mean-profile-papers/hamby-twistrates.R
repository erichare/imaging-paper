library(XML)
ptm <- proc.time()

# Loop through the vector, adding one
twist <- getTwist(path="~/CSAFE/Bullets/Hamby252_3DX3P1of2/Br1 Bullet 1-5.x3p")
# Stop the clock
proc.time() - ptm

library(dplyr)

knowns <- dir("~/CSAFE/Bullets/Hamby252_3DX3P1of2/", pattern="x3p", full.names=TRUE)
unknowns <- dir("~/CSAFE/Bullets/Hamby252_3DX3P2of2/", pattern="x3p", full.names=TRUE)

pattern <- "Bullet C"
twistlist <- c(knowns, unknowns) %>% 
  grep(pattern=pattern, x=., value=TRUE) %>% 
  purrr::map_df(getTwist, .id="path")

twistlist$bulletland <- with(twistlist, gsub(".*//(.*).x3p", "\\1", path))

file <- "mean-profile-papers/twist.csv"
fileexist = file.exist(file)
write.table(twistlist, file=file, sep="\t", header=!fileexist,
            row.names=FALSE, append=fileexist)
