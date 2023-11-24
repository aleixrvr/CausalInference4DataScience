set.seed(1234)

n <- 100

z <- rnorm(n, sd=.5)
x <- rnorm(n, sd=.5)
y <- 3*x + 3*z + rnorm(n, sd=.5)

summary(lm(y ~ x))
summary(lm(y ~ x + z))

z <- rnorm(n, sd=.5)
x <- z + rnorm(n, sd=.1)
y <- 3*x + rnorm(n, sd=.5)

summary(lm(y ~ x))
summary(lm(y ~ x + z))
