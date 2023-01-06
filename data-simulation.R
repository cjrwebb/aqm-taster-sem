library(tidyverse)
library(faux)
library(msm)
library(lavaan)
library(simsem)

n <- 5e3

# Annual Gross Income: exponential distribution (in 1000s)
income <- round(exp(rtnorm(n, 10, 1, lower = 8.517193, upper = 12.20607)) / 1000, 1)


# Investment: Must be a variable percentage of income, with a smaller percentage among
# high earners
pc_invest <- rtnorm(n, 0.4 - 0.001 * income, 0.05, lower = 0.05, upper = 0.6)
investment <- (pc_invest * income) + rtnorm(n, 0, 2, lower = -1)


# Family stress: unobserved normal factor, a product of income and investment 
stress <- rnorm(n, 0 - 0.4*scale(income) - 0.2*scale(investment), 1 - sum(0.4^2, 0.2^2))

# Manifest variables of family stress: 
#   - In the last month, felt that caring for children takes up more time and energy than
#     they have to give?
#   - In the last month, ever had an argument with a co-parent about their parenting 
#     practices that didn't lead to a good resolution?
#   - In the last month, felt like the responsibilities of being a parent are overwhelming?
#   - In the last week, felt like co-parent doesn't do enough to help with raising child(ren)?
#   - In the last month, had an argument with a co-parent in front of the children?

# Make constant around 20% (log 0.2/0.8)

stress_q1 <- round(rtnorm(n, 0 + 0.4*scale(income) + 0.2*scale(investment), 1 - sum(0.4^2, 0.2^2), lower = -3, upper = 3), 0)
stress_q2 <- round(rtnorm(n, 0 + 0.4*scale(income) + 0.2*scale(investment), 1 - sum(0.4^2, 0.2^2), lower = -3, upper = 3), 0)
stress_q3 <- round(rtnorm(n, 0 + 0.4*scale(income) + 0.2*scale(investment), 1 - sum(0.4^2, 0.2^2), lower = -3, upper = 3), 0)
stress_q4 <- round(rtnorm(n, 0 + 0.4*scale(income) + 0.2*scale(investment), 1 - sum(0.4^2, 0.2^2), lower = -3, upper = 3), 0)
stress_q5 <- round(rtnorm(n, 0 + 0.4*scale(income) + 0.2*scale(investment), 1 - sum(0.4^2, 0.2^2), lower = -3, upper = 3), 0)

mvsim_dat <- faux::rnorm_multi(
  n = n,
  mu = c(
         income     = 0,
         investment = 0,
         stress_q1  = 0, 
         stress_q2  = 0, 
         stress_q3  = 0, 
         stress_q4  = 0, 
         stress_q5  = 0),
  r = c( 1,    0.7, -0.5, -0.5, -0.5, -0.5, -0.5,
         0.7,    1, -0.5, -0.5, -0.5, -0.5, -0.5,
        -0.5, -0.5,    1,  0.8,  0.7,  0.6,  0.5,
        -0.5, -0.5,  0.8,    1,  0.6,  0.9,  0.7,
        -0.5, -0.5,  0.7,  0.6,    1,  0.6,  0.8,
        -0.5, -0.5,  0.6,  0.9,  0.6,    1,  0.8,
        -0.5, -0.5,  0.5,  0.7,  0.8,  0.8,    1),
  varnames = c("income", "investment", "stress_q1", "stress_q2", "stress_q3", "stress_q4", "stress_q5")
)

cfa_mod <- "s =~ stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5
"

stress_cfa <- sem(cfa_mod, 
    data = mvsim_dat) 

stress_lv <- lavPredict(stress_cfa)[,1]


# post data transformation (remove standardisation)
mvsim_dat$stress_q1 <- round(mvsim_dat$stress_q1, 0) 
mvsim_dat$stress_q2 <- round(mvsim_dat$stress_q2, 0) 
mvsim_dat$stress_q3 <- round(mvsim_dat$stress_q3, 0) 
mvsim_dat$stress_q4 <- round(mvsim_dat$stress_q4, 0) 
mvsim_dat$stress_q5 <- round(mvsim_dat$stress_q5, 0) 

# Stirling Children's Wellbeing Scale: truncated discrete normal between 12 and 60, mean = 44
scws <- round(rtnorm(n = n, mean = 44 - 3 * stress_lv + 1*scale(mvsim_dat$income) + 2*scale(mvsim_dat$investment), sd = 6, upper = 60, lower = 12), 0)

dat <- tibble(
  std_income = mvsim_dat$income,
  std_investment = mvsim_dat$investment,
  stress_q1 = mvsim_dat$stress_q1,
  stress_q2 = mvsim_dat$stress_q2,
  stress_q3 = mvsim_dat$stress_q3,
  stress_q4 = mvsim_dat$stress_q4,
  stress_q5 = mvsim_dat$stress_q5,
  scws = scws
)


# un-standardise income and investment
dat <- dat %>%
  mutate(
    income = (std_income * 5) + 20,
    investment = (std_investment * 3) + 10
  )



cov(dat$std_income, dat$std_investment)
cov(dat$income, dat$investment)
sd(dat$income)
sd(dat$investment)
sd(dat$scws)

sem_mod <- "

# Create the latent variable family stress (note: NA* is used to free)
# factor loading, and later stress ~~ resvar*stress is used to standardise
# the latent variable so that it has a mean of 0 and standard deviation of 1
stress =~ NA*stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5

# Regress stress on income and investment, and label the paths a1 & a2
stress ~ a1*income + a2*investment

# Regress investment on income and label the path a3
investment ~ a3*income

# Regress child wellbeing on the Stirling Child Wellbeing Score on stress, 
# income, and investment, label paths b1, c1, and b2
scws ~ b1*stress + c1*income + b2*investment

# make stress latent variable standardised (total variance = 1), 
# residual variance = 1 - beta^2
stress ~~ resvar*stress

# Manual calculation of standardised coefficients
# (note: you can estimate the model and use 1 - the R-squared, then
# add this manually for calculating resvar, or use std.lv = TRUE)
# standardised coefficients = beta*sd(x)/sd(y)
# covariance between predictors = 2*beta1*cov(x1,x2)*a2
std_a1 := a1*(5.044198/1)
std_a2 := a2*(3.008222/1)
std_a3 := a3*(5.044198/3.008222)
std_b1 := b1*(1/7.22341)
std_b2 := b2*(3.008222/7.22341)
std_c1 := c1*(5.044198/7.22341)
resvar == 1 - (std_a1^2 + std_a2^2 + 2*a1*10.67699*a2)

# Calculate indirect effects from income through investment,
# from income through investment and stress
# From income through stress, and for income total (all indirect and direct)
inc_inv := a3*b2
inc_inv_st := a3*a2*b1
inc_str := a1*b1
inc_tot := c1+inc_inv+inc_inv_st+inc_str

# standardised indirect effects
std_inc_inv    := std_a3*std_b2
std_inc_inv_st := std_a3*std_a2*std_b1
std_inc_str    := std_a1*std_b1
std_inc_tot    := std_c1+std_inc_inv+std_inc_inv_st+std_inc_str

# Absolute difference between total indirect income effects and stress effects
# -1* is necessary here because b1 is negative and std_inc_tot is positive
std_diff_incind_st := std_inc_tot - -1*std_b1

"

sem_out <- sem(sem_mod, data = dat)

summary(sem_out, standardized = TRUE, rsquare = TRUE)
parameterestimates(sem_out)
# Save data for worked example
#write_csv(dat, "data/example-data-sem.csv")


# Latent growth curve model example: margarine consumption etc

t = 1:10
mu_t_margarine <- matrix(nrow = 50, ncol = 10)
mu_t_divorce <- matrix(nrow = 50, ncol = 10)

for (i in 1:50) {
  
  mus <- rnorm_multi(
    n = 1,
    mu = c(3.6, 5.5),
    sd = c(0.5, 0.75),
    r = c(1, 0.4,
          0.4, 1),
    varnames = c("margarine_mu", "divorce_mu")
  )
  
  slopes <- rnorm_multi(
    n = 1,
    mu = c(0.2, 0.2),
    sd = c(0.03, 0.06),
    r = c(1, 0.6,
          0.6, 1),
    varnames = c("margarine_slope", "divorce_slope")
  )
  
  mu_t_margarine[i, ] <- mus$margarine_mu - slopes$margarine_slope*t
  mu_t_divorce[i, ] <- mus$divorce_mu - slopes$divorce_slope*t

  }


# correlated noise
noise_margarine <- matrix(nrow = 50, ncol = 10)
noise_divorce <- matrix(nrow = 50, ncol = 10)

for (i in 1:10) {
  noise <- rnorm_multi(
    n = 50,
    mu = c(0, 0),
    sd = c(0.3, 0.3),
    r = c(1, -0.1,
          -0.1, 1),
    varnames = c("margarine_e", "divorce_e")
  )
  
  noise_margarine[,i] <- noise$margarine_e
  noise_divorce[,i] <- noise$divorce_e
  
}



margarine <- as_tibble(ifelse(mu_t_margarine + noise_margarine < 0, 0, mu_t_margarine + noise_margarine))
names(margarine) <- paste0("margarine_", seq(1, 10, 1))

divorce <- as_tibble(ifelse(mu_t_divorce + noise_divorce < 0, 0, mu_t_divorce + noise_divorce))
names(divorce) <- paste0("divorce_", seq(1, 10, 1))

sim_margdiv <- bind_cols(margarine, divorce)



# correlated residual noise within time points

div_marg_growthmod <- "

i_div =~ 1*divorce_1 + 1*divorce_2 + 1*divorce_3 + 1*divorce_4 + 1*divorce_5 + 1*divorce_6 + 1*divorce_7 + 1*divorce_8 + 1*divorce_9 + 1*divorce_10
s_div =~ 0*divorce_1 + 1*divorce_2 + 2*divorce_3 + 3*divorce_4 + 4*divorce_5 + 5*divorce_6 + 6*divorce_7 + 7*divorce_8 + 8*divorce_9 + 9*divorce_10

i_marg =~ 1*margarine_1 + 1*margarine_2 + 1*margarine_3 + 1*margarine_4 + 1*margarine_5 + 1*margarine_6 + 1*margarine_7 + 1*margarine_8 + 1*margarine_9 + 1*margarine_10
s_marg =~ 0*margarine_1 + 1*margarine_2 + 2*margarine_3 + 3*margarine_4 + 4*margarine_5 + 5*margarine_6 + 6*margarine_7 + 7*margarine_8 + 8*margarine_9 + 9*margarine_10

i_div ~~ s_div + i_marg + s_marg
s_div ~~ i_marg + s_marg
i_marg ~~ s_marg


"

lavaan::growth(div_marg_growthmod, data = sim_margdiv) %>% summary(standardized = TRUE)



div_marg_growthmod <- "

i_div =~ 1*divorce_1 + 1*divorce_2 + 1*divorce_3 + 1*divorce_4 + 1*divorce_5 + 1*divorce_6 + 1*divorce_7 + 1*divorce_8 + 1*divorce_9 + 1*divorce_10
s_div =~ 0*divorce_1 + 1*divorce_2 + 2*divorce_3 + 3*divorce_4 + 4*divorce_5 + 5*divorce_6 + 6*divorce_7 + 7*divorce_8 + 8*divorce_9 + 9*divorce_10

i_marg =~ 1*margarine_1 + 1*margarine_2 + 1*margarine_3 + 1*margarine_4 + 1*margarine_5 + 1*margarine_6 + 1*margarine_7 + 1*margarine_8 + 1*margarine_9 + 1*margarine_10
s_marg =~ 0*margarine_1 + 1*margarine_2 + 2*margarine_3 + 3*margarine_4 + 4*margarine_5 + 5*margarine_6 + 6*margarine_7 + 7*margarine_8 + 8*margarine_9 + 9*margarine_10

i_div ~~ s_div + i_marg + s_marg
s_div ~~ i_marg + s_marg
i_marg ~~ s_marg

# Correlated within-time point residuals (fixed to be equal)
divorce_1 ~~ resid_cor*margarine_1
divorce_2 ~~ resid_cor*margarine_2
divorce_3 ~~ resid_cor*margarine_3
divorce_4 ~~ resid_cor*margarine_4
divorce_5 ~~ resid_cor*margarine_5
divorce_6 ~~ resid_cor*margarine_6
divorce_7 ~~ resid_cor*margarine_7
divorce_8 ~~ resid_cor*margarine_8
divorce_9 ~~ resid_cor*margarine_9
divorce_10 ~~ resid_cor*margarine_10


"

lavaan::growth(div_marg_growthmod, data = sim_margdiv) %>% summary(standardized = TRUE)

# write_csv(sim_margdiv, "data/margarines_divorce.csv")


margdiv_long <- sim_margdiv %>%
  pivot_longer(cols = everything()) %>%
  mutate(t = as.numeric(str_split(name, "_", simplify = TRUE)[,2]),
         name = str_split(name, "_", simplify = TRUE)[,1])

margdiv_long %>%
  ggplot() + 
  geom_point(aes(x = t, y = value, col = name))


