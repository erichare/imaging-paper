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
pattern <- "Bullet [DE]"
pattern <- "Bullet [FHJ]"
pattern <- "Bullet [LMQ]"
pattern <- "Bullet [SUX]"
pattern <- "Bullet [YZ]"
pattern <- "Br[12] "
pattern <- "Br[345] "
pattern <- "Br[678] "
pattern <- "Br9 "
pattern <- "Br10 "
twistlist <- c(knowns, unknowns) %>% 
  grep(pattern=pattern, x=., value=TRUE) %>% 
  purrr::map_df(getTwist, twistlimit=c(-2,0)*1.5625)
# .id only keeps track of the order of the results at the moment. that's useless.
twistlist$path <- c(knowns, unknowns) %>% 
  grep(pattern=pattern, x=., value=TRUE)

twistlist$bulletland <- with(twistlist, gsub(".*//(.*).x3p", "\\1", path))

file <- "/Users/heike/papers/2015-imaging-paper/mean-profile-papers/twists-constrained.csv"
fileexist = file.exists(file)
write.table(twistlist, file=file, sep=",", col.names=!fileexist,
            row.names=FALSE, append=fileexist)


twists <- read.csv("mean-profile-papers/twists.csv")
qplot(twist, min.r.squared, data=twists)
twists$bullet <- gsub("(.*)-[0-9]*","\\1",twists$bulletland)
qplot(twist, min.r.squared, data=twists) + facet_wrap(~bullet)
summary(twists$twist)
twists$constraint <- "none"
twists$method <- "q75"
write.csv(twists, "mean-profile-papers/twists.csv", row.names=FALSE)
