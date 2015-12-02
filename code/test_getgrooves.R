library(x3pr)
library(x3prplus)
library(ggplot2)

setwd("~/Downloads/Hamby252_3DX3P1of2/")
for (file in dir()) {
    x <- get_grooves(get_bullet(file), smoothfactor = 41)
    cat(file, "\n")
    print(x$plot)
    value <- readline("Press Enter to Continue, or q to Quit")
    if (value == "q") break;
}
