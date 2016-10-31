library(bulletr)
library(ggplot2)

for (file in dir("images/Hamby (2009) Barrel/bullets")) {
    mybullet <- get_crosscut(file.path("images/Hamby (2009) Barrel/bullets", file), x = 150)
    x <- get_grooves(mybullet)
    cat(file, "\n")
    my.loess <- fit_loess(mybullet, x)
    result <- get_peaks(my.loess$data, smoothfactor = 15)
    print(result$plot)
    value <- readline("Press Enter to Continue, or q to Quit")
    if (value == "q") break;
}
