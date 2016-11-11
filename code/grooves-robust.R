library(bulletr)

path <- "~/CSAFE/Bullets/Cary Persistence/bullets"
lands <- dir(path)

i <- 1
land <- read_x3p(file.path(path, lands[i]))
plot_3d_land(bullet=land)


grooves <- file.choose()
grooves <- readr::read_csv(grooves)

library(dplyr)
library(ggplot2)
library(MASS)


res <- grooves %>%  tidyr::nest(-bullet)


res$grooves <- res$data %>% purrr::map(
  .f = function(d) {
    rr <- rlm(data=d, groove_right~x)
    rl <- rlm(data=d, groove_left~x)
    data.frame(groove_right_pred=predict(rr, d), groove_left_pred= predict(rl,d))
  }
)

grooves2 <- res %>% unnest()
write.csv(grooves2, file.choose(), row.names=FALSE)

####################
# some visualizations

land <- grooves %>% filter(bullet == "app/images/Hamby252_3DX3P1of2//Br1 Bullet 1-2.x3p")

rr <- rlm(data=land, groove_right~x)

land %>%
  ggplot(aes(x = x, y = groove_right)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  geom_abline(intercept = rr$coefficients[1], slope = rr$coefficients[2], colour="red") 

land %>% 
  ggplot(aes(x = predict(rr, land), y = x)) + geom_point() +
  geom_point(aes(x = groove_right), colour="red") +
  geom_point(aes(x = groove_left), colour="red") 
