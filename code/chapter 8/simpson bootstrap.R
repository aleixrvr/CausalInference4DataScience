library(ggplot2)
set.seed(1234)

details <- list()

details[[1]] <- list(
  treatment = "A",
  size = "small",
  patients_n = 87,
  recovered_n = 81
)  
details[[2]] <- list(
  treatment = "A",
  size = "large",
  patients_n = 263,
  recovered_n = 192
)  
details[[3]] <- list(
  treatment = "B",
  size = "small",
  patients_n = 270,
  recovered_n = 234
)  
details[[4]] <- list(
  treatment = "B",
  size = "large",
  patients_n = 80,
  recovered_n = 50
)  


treatments <- c()
sizes <- c()
recovered <- c()

for( i in 1:4){
  treatments <- c(treatments, 
    rep(details[[i]]$treatment, details[[i]]$patients_n))
  sizes <- c(sizes, 
    rep(details[[i]]$size, details[[i]]$patients_n))
  recovered_ <- rep(0, details[[i]]$patients_n)
  recovered_[1:details[[i]]$recovered_n] <- 1
  recovered <- c(recovered, recovered_)  
}

df <- data.frame(treatments, sizes, recovered)

adjustment <- function(t, o, z, t0){
  ind_t0 <- t == t0
  z_values <- unique(z)
  
  adjusted_prob <- 0
  for(z_ in z_values){
    ind_z_ <- z == z_
    ind <- ind_t0 & ind_z_
    adjusted_prob <- adjusted_prob + mean(o[ind])*mean(ind_z_)
  }
  
  return(adjusted_prob)
}

adjustment(df$treatments, df$recovered, df$sizes, 'A')
adjustment(df$treatments, df$recovered, df$sizes, 'B')

bootstrap_n <- 1000
ates <- c()
for(i in 1: bootstrap_n){
  resample <- sample(nrow(df), replace = TRUE)
  df_resample <- df[resample, ]
  do_t_A <- adjustment(df_resample$treatments, 
    df_resample$recovered, df_resample$sizes, 'A')
  do_t_B <- adjustment(df_resample$treatments, 
    df_resample$recovered, df_resample$sizes, 'B')
  ates <- c(ates, do_t_A - do_t_B)
}

results <- data.frame(ate = ates)

qplot(ate, data = results, geom = "histogram")

quantile(ates)
mean(ates > 0)
