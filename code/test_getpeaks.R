library(x3pr)
library(x3prplus)
library(ggplot2)

for (file in dir("app/images/Hamby252_3DX3P1of2")) {
    mybullet <- get_crosscut(file.path("app/images/Hamby252_3DX3P1of2", file), x = 150)
    x <- get_grooves(mybullet)
    cat(file, "\n")
    my.loess <- fit_loess(mybullet, x)
    result <- get_peaks(my.loess$data, smoothfactor = 15)
    print(result$plot)
    value <- readline("Press Enter to Continue, or q to Quit")
    if (value == "q") break;
}
