
set.seed(2021)
n <- 100

ghosts <- sample(2:6, n, replace = TRUE) 
bonuses <- 6 - ghosts + sample(c(-1, 0, 1), n, replace = TRUE)
error <- rnorm(n, sd=0.1)

time <- .5 * ghosts -2 * bonuses + 20 + error

summary(lm(time~bonuses))
summary(lm(time~ghosts + bonuses))


library(ggplot2)

df <- data.frame(ghosts, bonuses)

ggplot(df, aes(ghosts, bonuses)) +
  geom_jitter(height=0.1, width=0.1)
