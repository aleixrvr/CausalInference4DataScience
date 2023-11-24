library(AER)
library(ggplot2)

set.seed(1234)
n <- 1000
treatment_effect <- -2
instrument_effect <- 0.7

confounder <- rbinom(n, 1, 0.3)
instrument <- rbinom(n, 1, 0.5)
treatment <- as.numeric(runif(n) <= instrument_effect*instrument + 0.4*confounder)
outcome <- 30 + treatment_effect*treatment + 10 * confounder + rnorm(n, sd=2)
df <- data.frame(instrument, treatment, outcome)

lm(outcome~treatment, data=df)
print(mean(df[df$treatment==1, 'outcome']) - mean(df[df$treatment==0, 'outcome']))

cov(outcome, instrument)/cov(treatment, instrument)

model <- ivreg(outcome~treatment|instrument, data=df)
summary(model)

model_1 <- lm(treatment~instrument, data=df)
treatment_prediction <- predict(model_1, data=df)
lm(outcome~treatment_prediction)




generate_data <- function(instrument_effect, n){
  confounder <- rbinom(n, 1, 0.3)
  instrument <- rbinom(n, 1, 0.5)
  treatment <- as.numeric(runif(n) <= instrument_effect*instrument + 0.4*confounder)
  outcome <- 30 + + treatment_effect*treatment + 10 * confounder + rnorm(n, sd=2)
  
  data.frame(instrument, treatment, outcome)
}

estimate_impact <- function(df){
  cov(df$outcome, df$instrument)/cov(df$treatment, df$instrument)
}

sim_n <- 1000
instrument_effect_1 <- 0.3
estimates <- c()
for(i in 1:sim_n){
  df <- generate_data(instrument_effect_1, n)
  estimates <- c(estimates, estimate_impact(df))
}

results <- data.frame(estimates=estimates, instrument_effect=instrument_effect_1)

instrument_effect_2 <- 0.1
estimates <- c()
for(i in 1:sim_n){
  df <- generate_data(instrument_effect_2, n)
  estimates <- c(estimates, estimate_impact(df))
}

results <- rbind(
  results, data.frame(estimates=estimates, instrument_effect=instrument_effect_2)
)
results$instrument_effect <- factor(results$instrument_effect)

ggplot(results, aes(estimates, fill=instrument_effect)) +
  geom_density(alpha=0.2) +
  geom_vline(xintercept = treatment_effect) +
  ggtitle('Distribution of the estimation of the causal effect with IVs')


library(DoubleML)
library(mlr3learners)

set.seed(12345)
df <- generate_data(instrument_effect = 0.5, n)
df['x'] <- 1

obj_dml_data = DoubleMLData$new(
  df, y_col="outcome", d_col = "treatment", z_cols= "instrument"
)

ml_g = lrn("regr.lm")
ml_m = lrn("classif.log_reg")
ml_r = ml_m$clone()

iv_2 = DoubleMLIIVM$new(obj_dml_data, ml_g, ml_m, ml_r)
iv_2$fit()
print(iv_2)
cov(df$outcome, df$instrument)/cov(df$treatment, df$instrument)

write.csv(df, 'chapter 9/df.csv', quote = FALSE, row.names = FALSE)
