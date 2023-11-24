n <- 100000

confounder <- rbinom(n, 1, 0.3)
instrument <- rbinom(n, 1, 0.5)
treatment <- as.numeric(runif(n) <= 1/(1+exp(-(5 - 3*confounder + 5*instrument))))
# glm(D~I + Z, family='binomial')
outcome <- 30 - 2*treatment - 10 * confounder + rnorm(n, sd=2)


C <- rbinom(n, 1, 0.3)
I <- rbinom(n, 1, 0.5)
D <- as.numeric(runif(n) <= 1/(1+exp(-(5 - 3*C + 5*I))))
glm(D~I + C, family='binomial')
Y <- 30 - 2*D - 10 * C + rnorm(n, sd=2)




library(DoubleML)
library(mlr3)
library(mlr3learners)

df <- data.frame(I, D, Y)
df['X'] <- 1


obj_dml_data = DoubleMLData$new(
  df, y_col="Y", d_col = "D", z_cols= "I"
)

learner = lrn("regr.lm")
ml_l = learner$clone()
ml_m = learner$clone()
ml_r = learner$clone()

iv_1 = DoubleMLPLIV$new(obj_dml_data, ml_l, ml_m, ml_r)
iv_1$fit()

ml_l = lrn("regr.lm")
ml_m = lrn("classif.log_reg")
ml_r = lrn("classif.log_reg")

iv_2 = DoubleMLIIVM$new(obj_dml_data, ml_l, ml_m, ml_r)
iv_2$fit()
print(iv_2)
print(cov(outcome, instrument)/cov(treatment, instrument))


library(AER)

print(iv_1)
# print(iv_2)
ivreg(Y~D | I)
cov(I, Y)/cov(I, D)
