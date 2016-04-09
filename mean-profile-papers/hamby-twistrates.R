library(XML)
ptm <- proc.time()

# Loop through the vector, adding one
twist <- getTwist(path="~/CSAFE/Bullets/Hamby252_3DX3P1of2/Br1 Bullet 1-5.x3p")
# Stop the clock
proc.time() - ptm

library(dplyr)

knowns <- dir("~/CSAFE/Bullets/Hamby252_3DX3P1of2/", pattern="x3p", full.names=TRUE)
unknowns <- dir("~/CSAFE/Bullets/Hamby252_3DX3P2of2/", pattern="x3p", full.names=TRUE)

pattern <- "Bullet B"
pattern <- "Bullet C"
pattern <- "Br[12] "
twistlist <- c(knowns, unknowns) %>% 
  grep(pattern=pattern, x=., value=TRUE) %>% 
  purrr::map_df(getTwist)
# path only keeps track of the order of the results at the moment. that's useless.
twistlist$path <- c(knowns, unknowns) %>% 
  grep(pattern=pattern, x=., value=TRUE)

twistlist$bulletland <- with(twistlist, gsub(".*//(.*).x3p", "\\1", path))

file <- "mean-profile-papers/twists.csv"
fileexist = file.exists(file)
write.table(twistlist[,c(4,3,1,2)], file=file, sep=",", col.names=!fileexist,
            row.names=FALSE, append=fileexist)
