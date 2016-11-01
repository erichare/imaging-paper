library(bulletr)
library(plotly)

mybullet <- read_x3p("images/Hamby (2009) Barrel/bullets/Br1 Bullet 1-1.x3p")
datamat <- mybullet$surface.matrix

plot_ly(z = ~datamat) %>% add_surface()

mybullet2 <- read_x3p("images/Hamby (2009) Barrel/bullets/br1_1_land1.x3p", transpose = TRUE)
datamat2 <- mybullet2$surface.matrix

plot_ly(z = ~datamat2) %>% add_surface()
