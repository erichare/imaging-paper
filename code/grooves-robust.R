grooves <- readr::read_csv(file.choose())

library(dplyr)
library(ggplot2)
library(MASS)


res <- grooves %>%  tidyr::nest(-bullet)

i <- 1
res$grooves <- res$data %>% purrr::map(
  .f = function(d) {
    groove_right_pred = d$groove_right
    groove_left_pred = d$groove_left
    
    d <- d %>% mutate(
      groove_right = replace(groove_right, groove_right == max(groove_right), NA),
      groove_left = replace(groove_left, groove_left == min(groove_left), NA)
    )
    if (!all(is.na(d$groove_right))) {
      rr <- rlm(data=d, groove_right~x)
      groove_right_pred=predict(rr, d)
    }
    if (!all(is.na(d$groove_left))) {
      rl <- rlm(data=d, groove_left~x)
      groove_left_pred= predict(rl,d)
    }
    data.frame(groove_left_pred, groove_right_pred, 
               right_twist=rr$coefficients[2], left_twist= rl$coefficients[2])
  }
)

grooves2 <- res %>% unnest()
write.csv(grooves2, file.choose(), row.names=FALSE)


grooves2 %>% ggplot(aes(x = groove_left, y = groove_left_pred)) + geom_point()
####################
# some visualizations

land <- grooves %>% filter(bullet == "images/Hamby (2009) Barrel/bullets/Br10 Bullet 1-1.x3p")

rr <- rlm(data=land, groove_right~x)
rl <- rlm(data=land, groove_left~x)

land %>%
  ggplot(aes(x = x, y = groove_right)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  geom_abline(intercept = rr$coefficients[1], slope = rr$coefficients[2], colour="red") 

land %>% 
  ggplot(aes(x = predict(rr, land), y = x)) + 
  geom_point() +
  geom_point(aes(x = predict(rl, land))) +
  geom_point(aes(x = groove_right), colour="red") +
  geom_point(aes(x = groove_left), colour="red") 
