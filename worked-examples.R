################# ----- Advanced Quantitative Methods SEM & LGM Taster Workbook

#' This workbook was written by Dr. Calum Webb to provide a very basic taster 
#' introduction to Structural Equation Modelling and Latent Growth Modelling in
#' R using the Lavaan package.

library(tidyverse) # The tidyverse includes plotting and data tidying tools
library(lavaan) # Lavaan is a package for structural equation modelling
library(dagitty) # Dagitty allows you to draw causal diagrams and calculate adjustment sets
library(lavaanPlot) # lavaanPlot allows you to plot lavaan objects

# You will need to install the above packages if they are not yet installed on your
# system:
# install.packages(c("tidyverse", "lavaan", "dagitty", "lavaanPlot"))


# Start by reading in the example data
child_data <- read_csv("data/example-data-sem.csv")

# This simulated dataset includes:
# Standardised and unstandardised (£1000s) measures of income and spending on
# activities related to children's development and wellbeing
# Answers to a 5-question family stress survey (10-point scale, centered at 5)
# Children's scores on the Stirling Child Wellbeing Scale (ranges 12-60) (scws)
summary(child_data)

#' Let's start by drawing a causal diagram between income, investment, and child welbeing
#' (A simple mediation model)
simple_dag <- dagitty("dag{Income -> {Investment Wellbeing} -> Wellbeing}")
plot(simple_dag)

#' A more complex dag incorporating family stress as a latent variable
complex_dag <- dagitty("dag{
                       Income -> {Investment Wellbeing Stress} -> {Wellbeing Stress}
                       Stress [unobserved]}")
plot(complex_dag)



# A simple structural equation model --------------------------------------

#' The first model can be built up in lavaan syntax. 

simple_sem <- "
  
  # The tilde symbol is used to declare a regression
  # Here, scws is being regressed on income and investment,
  # and investment is being regressed on income
  scws ~ income + investment
  investment ~ income

"

# The lavaan sem function can be used to estimate the model
simple_out <- lavaan::sem(simple_sem, data = child_data)

# The results from the model can be printed using the summary() function
summary(simple_out, standardized = TRUE, rsquare = TRUE)

# For basic models, the lavaanPlot package can express the model in a diagram
lavaanPlot::lavaanPlot(model = simple_out, coefs = TRUE)

#' The regression coefficients can be interpreted as usual:
#' An increase of 1 in income (£1000) was associated with a 0.42 (£420) increase in
#' spending on children (investment).
#' An increase of 1 in income was associated with an increase of 0.32 points on the
#' SCWS wellbeing scale, holding investment constant.
#' An increase of 1 in investment (£1000) was associated with an increase of 0.92 points
#' on the SCWS wellbeing scale, holding income constant.

#' However, the indirect effect of income mediated through investment can also be 
#' calculated by multiplying the two paths together (income -> investment, investment -> scws).
#' This is the indirect effect of income on wellbeing through its average impact on investment.

0.421 * 0.920

#' In other words, the impact of a £1000 increase in income on children's wellbeing
#' via its effect on investment is a 0.38732 point increase in wellbeing score. This is 
#' equal to a:

(0.421 * 0.920) / sd(child_data$scws)

#' change in y of 0.053 standard deviations in wellbeing score or partial correlation 
#' coefficient of:

(0.421 * 0.920) * (sd(child_data$income) / sd(child_data$scws))

#' Around a 0.265 standard deviation increase in wellbeing score for a 1 standard deviation
#' increase in income (around £5,000)

#' We could also work out the total impact of income via both its direct and indirect effect
#' by summing the coefficients:

(0.421 * 0.920) * (sd(child_data$income) / sd(child_data$scws)) + 0.324 * (sd(child_data$income) / sd(child_data$scws))

#' In other words, a 1 standard deviation increase in income is associated with a 0.487 
#' standard deviation increase in child wellbeing via both its indirect effect through
#' investment and its direct effect.

#' However, it's generally not very good practice to do these calculations outside of 
#' estimating the model (especially because this makes their significance etc. very
#' difficult to assess). You can calculate any generated quantities in lavaan code by
#' using the := operator: 

simple_sem <- "
  
  # The tilde symbol is used to declare a regression
  # Here, scws is being regressed on income and investment,
  # and investment is being regressed on income

  # The paths have been labelled a1 (income -> investment),
  # b1 (investment -> scws), and c1, sometimes called the 
  # direct effect (income -> scws)
  scws ~ c1*income + b1*investment
  investment ~ a1*income

  # generate the indirect effect and the total effect
  indirect := a1*b1
  total := indirect + c1

"

# The lavaan sem function can be used to estimate the model
simple_out <- lavaan::sem(simple_sem, data = child_data)

# The results from the model can be printed using the summary() function
summary(simple_out, standardized = TRUE, rsquare = TRUE)

# The std.all column gives us our standardised coefficients


# Latent variable estimation ----------------------------------------------

#' Our family stress questionnaire variables stress_q1 thru q5 measure aspects 
#' of a latent family stress construct on a centered 10-point scale.
#' We could include all questions as predictors, but these would be highly 
#' collinear and would erroneously make some measures of stress, and the overall
#' effect of stress, look weak, just like in a regular regression model:


simple_sem <- "
  
  # The tilde symbol is used to declare a regression
  # Here, scws is being regressed on income and investment,
  # and investment is being regressed on income

  # The paths have been labelled a1 (income -> investment),
  # b1 (investment -> scws), and c1, sometimes called the 
  # direct effect (income -> scws)
  scws ~ c1*income + b1*investment + stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5
  investment ~ a1*income

  # generate the indirect effect and the total effect
  indirect := a1*b1
  total := indirect + c1

"

simple_out <- lavaan::sem(simple_sem, data = child_data)
summary(simple_out, standardized = TRUE, rsquare = TRUE)


#' Alternatively, we could take a composite score of all of the stress variables (add
#' the scores together), but this only works well when all questions are on the same 
#' scale with similar variance. Moreover, a composite score is not necessarily a good
#' measure of the underlying construct, whereas how well latent variables measure an 
#' underlying construct can be empirically tested. 
#' We could pick one variable to be a proxy for all stress, but that would include all 
#' of the measurement error associated with that one specific variable. 
#' A more justifiable solution is to construct a latent 'stress' variable using the
#' =~ operator:


#' Here I have chosen to make the stress variable a standardised factor (it will have
#' a mean of 0 and a standard deviation of 1). This requires a little more work to set 
#' up, especially when the latent variable is endogenous (as we will see), but makes
#' interpretation a little easier (a 1 standard deviation increase in this latent
#' construct stress). By default, LVs use a marker variable (say stress q1) and 
#' take on the scale of that marker variable (e.g. ranging between -5 and +5, or 0-1).

simple_cfa <- "
  
  # construct the stress latent variable
  stress =~ NA*stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5

  # Set the variance of the stress LV to be 1 (standardised)
  stress ~~ 1*stress

"

simple_cfa_out <- lavaan::sem(simple_cfa, data = child_data)
summary(simple_cfa_out, standardized = TRUE, rsquare = TRUE, fit.measures = TRUE)

#' Testing the fit, reliability, and validity of latent variables is an enormous
#' topic in itself. Briefly, a variety of fit measures can be used to assess how
#' well a set of indicator or manifest variable measure an underlying construct,
#' and better scores on these statistics generally refer to how well the estimated
#' latent variable can reproduce the variance-covariance matrix across the indicators.
#' 
#' In this example, our CFI and SRMR statistics seem to indicate a satisfactory fit,
#' but the RMSEA and TLI fit statistics indicate a fairly poor fit. This may indicate
#' that a one-factor solution is not suitable. There may be two distinct types of
#' family stress captured by our questions, say, parenting stress and partner stress,
#' in which case the model is wrong, or we may have done a poor job developing our questions.
#' A full Exploratory Factor Analysis study can help us develop a better latent construct.
#' Alternatively, this might be partly a consequence of the discrete nature of the 
#' responses (technically a set of ordinal variables), so it may be more appropriate
#' to use probit links between the latent variable and the indicator variables (see
#' the ordered = TRUE argument in lavaan's functions). For a discussion of fit statistics,
#' see: http://www.davidakenny.net/cm/fit.htm
#' 
#' Regardless, we will continue with the one-factor linear solution for the sake
#' of demonstration.

# The stand = TRUE argument standardises the coefficients
lavaanPlot::lavaanPlot(model = simple_cfa_out, stand = TRUE, coefs = TRUE)

#' we can also plot the results of our confirmatory factor analysis as above, 
#' note the direction of the arrows. The latent variable that has been estimated
#' is predicting the indicator variables.

#' further, we can visualise the constructed variable scores for each observation:

stress_lv <- lavPredict(simple_cfa_out)
hist(stress_lv)

#' because we set it to be standardised, its mean should be approximately 0
#' and its standard deviation should be approximately 1
mean(stress_lv)
sd(stress_lv)


# Incorporating our SEM and LV --------------------------------------------

#' Once a latent variable has been created, it can be used like any other variable
#' in a structural equation model. Let's add the latent familt stress variable to
#' our earlier SEM to create the 'complex' SEM from before:

complex_sem <- "
  
  # construct the stress latent variable
  stress =~ NA*stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5

  # Set the variance of the stress LV to be 1 (standardised)
  stress ~~ 1*stress

  scws ~ d1*income + c1*investment + c2*stress
  investment ~ a1*income
  stress ~ a2*income + b2*investment

  # generate the indirect effects and the total effects
  indir_thru_inv := a1*c1
  indir_thru_str := a2*c2
  indir_thru_inv_str := a1*b2*c2
  direct_inc := d1
  total := indir_thru_inv + indir_thru_str + indir_thru_inv_str + direct_inc

"
complex_out <- lavaan::sem(complex_sem, data = child_data)
summary(complex_out, standardized = TRUE, rsquare = TRUE, fit.measures = TRUE)
# use the stand = TRUE argument for standardised coefficients to be shown
lavaanPlot::lavaanPlot(model = complex_out, coefs = TRUE, stand = TRUE)


#' However, we have something of a problem here: before, in our cfa model,
#' our stress variable was standardised, which made interpretation a little easier.
#' Now that it is an endogenous variable the syntax stress ~~ 1*stress sets the
#' *residual* variance to be 1, not the total variance. 
#' There are a few ways to make the residual variance equal one again (or we
#' can just read from the Std.lv column), but the most straightforward is 
#' probably just to subtract the R-square value for the stress latent variable
#' from 1 to adjust the end variance to sum to 1. 
#' First, lets check what the current sd is:

stress_lv_1 <- lavPredict(complex_out, type = "lv")
sd(stress_lv_1) # greater than 1

# Now let's subtract the R-squared value for stress (0.335) from 1, and fix 
# the variance of stress to be the result:

1 - 0.335

complex_sem <- "
  
  # construct the stress latent variable
  stress =~ NA*stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5

  # Set the variance of the stress LV to be 1 (standardised)
  stress ~~ 0.665*stress

  scws ~ d1*income + c1*investment + c2*stress
  investment ~ a1*income
  stress ~ a2*income + b2*investment

  # generate the indirect effects and the total effects
  indir_thru_inv := a1*c1
  indir_thru_str := a2*c2
  indir_thru_inv_str := a1*b2*c2
  direct_inc := d1
  total := indir_thru_inv + indir_thru_str + indir_thru_inv_str + direct_inc

"
complex_out <- lavaan::sem(complex_sem, data = child_data)
summary(complex_out, standardized = TRUE, rsquare = TRUE, fit.measures = TRUE)
# use the stand = TRUE argument for standardised coefficients to be shown
lavaanPlot::lavaanPlot(model = complex_out, stand = TRUE, coefs = TRUE)

# Check the LVs standard deviation:
stress_lv_2 <- lavPredict(complex_out)
sd(stress_lv_2)

#' This makes reporting our results related to latent stress easier because we
#' can invoke the empirical rule. Now let's interpret some of the results:

#' We can see that when considering income, investment, and stress outside of their
#' indirect pathways, that stress appears to be the largest contributor to child wellbeing
#' with a standardised coefficient of -0.361

#' However, when we consider all of the indirect and direct paths to child wellbeing
#' from income, we can see that the effect of income is actually stronger, with the total
#' effect (standardised) being 0.487. This is made up of an indirect effect through 
#' investment (0.187), an indirect effect through stress (0.116), and indirect effect
#' through investment and stress (0.078), and a direct effect (0.105).
#' 
#' In this case, the cost of decreasing stress by one standard deviation would have to 
#' be less than the cost of increasing income by 0.361/0.487 = 0.741 standard deviations
#' (approximately 0.741 * sd(dat$income) = £3.7k per annum) for it to be as effective as
#' simply increasing families' income by that amount.



# Latent growth modelling (parallel process) example ----------------------

#' The second dataset is some simulated data showing the spurious correlation between
#' margarine consumption and divorce rates over time across 50 US states:

div_dat <- read_csv("data/margarines_divorce.csv")

# Here is what the relationship between the two variables looks like over time
# (Including some code to convert the data from wide to long format for plotting)
div_dat_long <- div_dat %>%
  mutate(state = row_number(), .before = 1) %>%
  pivot_longer(cols = -1) %>%
  mutate(t = as.numeric(str_split(name, "_", simplify = TRUE)[,2]),
         name = str_split(name, "_", simplify = TRUE)[,1]) 

div_dat_long %>%
  ggplot() + 
  stat_smooth(aes(x = t, y = value, col = name, group = paste(name, state)), 
              method = "loess", se = F, size = 0.2, alpha = 0.5) +
  geom_smooth(aes(x = t, y = value, col = name)) +
  theme_minimal()

cor(div_dat_long$value[div_dat_long$name == "divorce"],
    div_dat_long$value[div_dat_long$name == "margarine"])
# A correlation of 0.579

#' Obviously, this correlation is likely cause by two things having a
#' similar trend over time (time can be though of as including a wide
#' variety of unmeasured confounders).
#' 
#' Sometimes the relationship between trends over time is interesting (e.g.
#' did local authorities that had steeper austerity cuts see bigger increases
#' in social care demand than local authorities that had shallower cuts),
#' and sometimes they are confounding nuisance we want to partition out of 
#' our model of interest.
#' 
#' The reason divorce rates and margarine consumption are positively correlated
#' over time is likely just a consequence of having similar, but unrelated trends:
#' Marriage is becoming less common, so fewer marriages are likely to be unhappy ones; 
#' people are increasingly concerned about trans-fats, so are in general using less
#' margarine, butter, and oils in their home cooking. These similar trends induce
#' a possible spurious correlation 
#' 
#' In this example, we'll look at first: 
#'  - Did states with steeper drops in margarine consumption have steeper drops in
#'    divorce rates?
#'  - After adjusting for the correlation in the trends over time, was there an 
#'    association between the margarine consumption and divorce rates residuals?
#'   


#' Latent growth models work by applying constraints to latent variables to make 
#' them capture a change-over-time function. This commonly is as simple as a 
#' linear function (defined by an intercept latent variable and a slope latent
#' variable), but can also be parameterised as a non-linear function (e.g.
#' a quadratic growth curve, an exponential growth curve, for example:
#' https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3169758/).
#' 
#' If you don't already have an appropriate growth function in mind, or the aim
#' of your study is to identify the most appropriate growth function, you would
#' typically compare the fit of several competing models. Here is a simple example
#' comparing a latent growth curve model that includes only varying intercepts (
#' no change over time), varying intercepts with a fixed slope function, varying
#' intercepts with varying slopes (by State), and varying intercepts with varying
#' slopes and a fixed quadratic function.


#' One caveat for latent growth models is that a mean structure needs to be specified
#' to make the mean and variance of the latent variable represent the intercept (and slope,
#' etc.) If you use the growth() function in lavaan, this will be added automatically to
#' *any* latent variables in the model. However, if you are using a mixture of latent
#' variables used to model a construct and latent variables used to measure change over
#' time, you will need to manually specify this mean structure for your latent growth
#' variables (see vintercept2 code) instead.
#' 
#' Here is some model syntax for an varying intercept only latent growth model
#' **for use with the growth()** function, which automatically sets the mean structure

vintercept <- "

  i_div =~ 1*divorce_1 + 1*divorce_2 + 1*divorce_3 + 1*divorce_4 + 
           1*divorce_5 + 1*divorce_6 + 1*divorce_7 + 1*divorce_8 + 
           1*divorce_9 + 1*divorce_10

"

# And here is an example that sets the mean structure manually. Below we will compare
# what happens if we use the vintercept object in a non-growth function like sem()
# compared to what happens if we use the growth() function and what happens if we use
# the vintercept2 model which manually specifies the mean structure.

vintercept2 <- "

  i_div =~ 1*divorce_1 + 1*divorce_2 + 1*divorce_3 + 1*divorce_4 + 
           1*divorce_5 + 1*divorce_6 + 1*divorce_7 + 1*divorce_8 + 
           1*divorce_9 + 1*divorce_10
  
  # Manual mean structure for latent growth curve modelling
  # Set means for indicator variables to 0
  divorce_1 + divorce_2 + divorce_3 + divorce_4 + divorce_5 + divorce_6 + divorce_7 + divorce_8 + divorce_9 + divorce_10 ~ 0*1
  
  # Estimate the mean of the intercept latent variable (by default would be fixed to 0)
  i_div ~ 1

"

# Here is some code for varying intercepts and a fixed slope. Note that the line
# s_div ~~ 0*s_div fixes the slope variance to 0 (meaning the same slope for every
# state). Obviously if a slope latent variable has no variance it cannot be correlated 
# with latent variables that do have variance. 

vintercept_and_fslope <- "

  i_div =~ 1*divorce_1 + 1*divorce_2 + 1*divorce_3 + 1*divorce_4 + 
           1*divorce_5 + 1*divorce_6 + 1*divorce_7 + 1*divorce_8 + 
           1*divorce_9 + 1*divorce_10
  s_div =~ 0*divorce_1 + 1*divorce_2 + 2*divorce_3 + 3*divorce_4 + 
           4*divorce_5 + 5*divorce_6 + 6*divorce_7 + 7*divorce_8 + 
           8*divorce_9 + 9*divorce_10
  
  # to fix the slopes to not vary, we fix the variance of the slopes latent variable 
  # to 0
  s_div ~~ 0*s_div

"

# A varying intercepts and varying slopes model just removes the constraint added
# above.

vintercept_and_vslope <- "

  i_div =~ 1*divorce_1 + 1*divorce_2 + 1*divorce_3 + 1*divorce_4 + 
           1*divorce_5 + 1*divorce_6 + 1*divorce_7 + 1*divorce_8 + 
           1*divorce_9 + 1*divorce_10
  s_div =~ 0*divorce_1 + 1*divorce_2 + 2*divorce_3 + 3*divorce_4 + 
           4*divorce_5 + 5*divorce_6 + 6*divorce_7 + 7*divorce_8 + 
           8*divorce_9 + 9*divorce_10

"

# Lastly, we can add a quadratic latent variable to create a non-linear growth 
# function (likely overfitting our data)

vintercept_and_vslope_and_fquad <- "

  i_div =~ 1*divorce_1 + 1*divorce_2 + 1*divorce_3 + 1*divorce_4 + 
           1*divorce_5 + 1*divorce_6 + 1*divorce_7 + 1*divorce_8 + 
           1*divorce_9 + 1*divorce_10
  s_div =~ 0*divorce_1 + 1*divorce_2 + 2*divorce_3 + 3*divorce_4 + 
           4*divorce_5 + 5*divorce_6 + 6*divorce_7 + 7*divorce_8 + 
           8*divorce_9 + 9*divorce_10
  q_div =~ 0*divorce_1 + 1*divorce_2 + 4*divorce_3 + 9*divorce_4 + 
           16*divorce_5 + 25*divorce_6 + 36*divorce_7 + 49*divorce_8 + 
           64*divorce_9 + 81*divorce_10
  
  q_div ~~ 0*q_div

"


# Before estimating and comparing the models, let's compare the growth()
# and sem() functions when we're using latent growth models...

i_only <- growth(vintercept, data = div_dat)
i_only_sem <- sem(vintercept, data = div_dat)

# Note here that the i_only model from the growth() function contains
# an intercept for i_div: 4.3 - indicating the mean divorce rate over
# the 10 years is around 4.3
summary(i_only) 
# we can check this using our long format data
mean(div_dat_long$value[div_dat_long$name == "divorce"])

# Now if we check the result from the sem() function we'll see that there 
# is no intercept mean, only a variance, because the mean is set by default
# to zero.
summary(i_only_sem)

# Now let's compare our manually specified mean structure:
i_only_manual <- sem(vintercept2, data = div_dat)
summary(i_only_manual)

#' The results are the same as when we used the growth() function. 
#' Another important piece of the output is the variance of the intercept 
#' latent variable: 0.622. This tells us that the intercept for divorce
#' rates across states had a standard deviation or around sqrt(0.622) = 0.789,
#' meaning that 95% of the state intercepts were around 4.3 ± 1.96*0.789 according
#' to the empirical rule. 

4.3 - 1.96*0.789
4.3 + 1.96*0.789

#' As before, we can visualise our latent variable using lavPredict

lvs <- lavPredict(i_only)
hist(lvs[, "i_div"], breaks = 30)

# Last, let's check some of our comparative model fit statistics that we'll
# use when comparing more complex models: the AIC and BIC are information
# criterion where smaller values equal better fitting models. 
AIC(i_only)
BIC(i_only)

#' Now let's run our other models. We may get some warning messages about
#' poor model fit
vi_fs <- growth(vintercept_and_fslope, data = div_dat) # varying intercept, fixed slopes
vi_vs <- growth(vintercept_and_vslope, data = div_dat) # varying intercept, varying slopes
vi_vs_fq <- growth(vintercept_and_vslope_and_fquad, data = div_dat) # varying intercept, varying slopes, fixed quadratic


#' Let's compare the AIC and BIC scores of each model before interpreting
#' our preferred one.
AIC(i_only, vi_fs, vi_vs, vi_vs_fq)
BIC(i_only, vi_fs, vi_vs, vi_vs_fq)

#' The AIC and BIC seem to suggest that the best fitting growth model is the 
#' one with varying intercepts and varying slopes, and the one with a fixed
#' quadratic component is likely overfit to the data.
#' 
#' Let's interpret this varying intercepts and varying slopes model for 
#' divorce rates before we add the growth in margarine consumption:

summary(vi_vs, standardized = TRUE, rsquare = TRUE)
lavaanPlot(model = vi_vs, coefs = TRUE)

#' Note here that, different to a multilevel growth model, the R-square
#' for each measurement of divorce is not fixed to be equal: the wide
#' format of data used means that, unless the residual variance of each
#' time point is fixed to be equal manually, the model will freely estimate
#' it. This can tell you something interesting like whether your growth 
#' function gets worse at predicting the variable at certain points of time
#' (like towards the ends of the data range).
#' 
#' 
#' Growth function:
#' 
#' The mean for the divorce rates intercept latent variable was 5.265 and the
#' mean for the divorce rates slope latent variable was -0.204. This suggests that
#' at the intercept point (when t = 0, in other words, in the first time point 
#' measurement) the mean divorce rate in states was around 5.265 per 1,000 marriages.
#' The slope mean suggests that the divorce rate is falling, on average, by -0.204
#' divorces per 1,000 per year; so in t = 2 we would expect divorce rates to be:
# 5.265 + -0.204*1 (remember t1 = 0) = 5.061 on average; at t = 3, 
# 5.265 + -0.204*2 = 4.857, and so on. 

#' The variance in the intercept suggests that intercepts had a standard deviation of
#' sqrt(0.472) = 0.687 per 1000, quantifying the state-level variation in divorce rates.
#' Meanwhile, the slope variance suggested that it was normally distributed around -0.2
#' with a standard deviation of sqrt(0.004) = 0.063, suggesting that in some states at the
#' lower tail of the distribution the divorce rates may have only been falling by 
#' -0.2 + 1.96*0.063 = -0.0765 divorces per 1,000 marriages and in others, at the upper tail
#' of the distribution, they were falling by as much as -0.324 divorces per 1,000 marriages.
#' 
#' We can visualise the slopes and intercepts we got using lavPredict. This is
#' somewhat easier to do in the base R plotting functions

lvs <- lavPredict(vi_vs)
lvs <- as_tibble(lvs)
x <- 0:9

plot(x = NULL, y = NULL, xlim = c(0, 9), ylim = c(0,10), xlab = "time", ylab = "Divorce rate")
for (i in 1:50) {
  lines(x = 0:9, y = lvs$i_div[[i]] + lvs$s_div[[i]]*x)
}


#' Lastly, we might be interested to see whether states that had higher intercepts had
#' steeper changes over time.

#' To do this we can look at the covariance between our intercept and our slope latent 
#' variables. Let's bring up the summary again:

summary(vi_vs, standardized = TRUE)

#' By default, all covariance between exogenous latent variables is estimated by lavaan, but
#' in many cases you would have to specify this manually using i_div ~~ s_div. 
#' 
#' The covariance between our intercept LV and slope LV is 0.008 which, on its own, isn't very
#' meaningful. However, we can view our standardised covariance (correlation) from the Std.lv 
#' or the Std.all column for a better idea of how strongly correlated the intercepts and slopes
#' for each state are. You should find that the correlation between the intercepts and slopes
#' was around 0.183, which suggests there was a fairly weak positive association between them.
#' States with higher divorce rates at t=0 tended to have slightly more positive slopes, 
#' suggesting their rates weren't going down as fast as those states that already had quite low
#' divorce rates at t = 0.


#' Now let's add a latent growth model for margarine consumption and look at the relationship
#' between change over time in margarine consumption and change over time in divorce rates. 
#' This is sometimes called a parallel processes model.
#' Usually before doing this you would repeat the process of finding the most appropriate
#' function for change in margarine consumption over time (feel free to try and do this
#' on your own), but for the sake of demonstration we'll jump straight to the PP model.

divorce_margarine_ppmod <- "

  i_div =~ 1*divorce_1 + 1*divorce_2 + 1*divorce_3 + 1*divorce_4 + 
           1*divorce_5 + 1*divorce_6 + 1*divorce_7 + 1*divorce_8 + 
           1*divorce_9 + 1*divorce_10
  s_div =~ 0*divorce_1 + 1*divorce_2 + 2*divorce_3 + 3*divorce_4 + 
           4*divorce_5 + 5*divorce_6 + 6*divorce_7 + 7*divorce_8 + 
           8*divorce_9 + 9*divorce_10
  
  i_marg =~ 1*margarine_1 + 1*margarine_2 + 1*margarine_3 + 1*margarine_4 + 
           1*margarine_5 + 1*margarine_6 + 1*margarine_7 + 1*margarine_8 + 
           1*margarine_9 + 1*margarine_10
  s_marg =~ 0*margarine_1 + 1*margarine_2 + 2*margarine_3 + 3*margarine_4 + 
           4*margarine_5 + 5*margarine_6 + 6*margarine_7 + 7*margarine_8 + 
           8*margarine_9 + 9*margarine_10
  
  # Estimate covariance between all latent variables
  i_div ~~ s_div + i_marg + s_marg
  s_div ~~ i_marg + s_marg
  i_marg ~~ s_marg

"

ppmod_out <- growth(divorce_margarine_ppmod, data = div_dat)
summary(ppmod_out, standardized = TRUE)


#' Our interpretation of the means of the latent variables is the same as before:
#' The mean intercept for divorce rates was 5.266 per 1000 marriages at t=0 with a
#' negative change over time of -0.204 per 1000 marriages per year increment.
#' The mean intercept for margarine consumption was 3.4kg per year with a negative 
#' change over time of around -0.215 kg per year.
#' 
#' Note that the change over time between the two variables is almost identical,
#' which is representative of the spurious correlations introduced by changes over
#' time. If we really want to know whether developments in margarine consumption 
#' are associated with changes in divorce rates over time, we should rather check
#' whether **states that had higher than average slopes in margarine consumption
#' also had higher than average slopes in divorce rates, and vide versa**.
#' 
#' The answer to this question can be found in the covariance/correlation between 
#' the varying slopes in margarine consumption and the varying slopes in divorce rates:
#' s_div ~~ s_marg. If you find that row in the output you should see that there
#' was a correlation of 0.357 -- interesting! Even though it's much lower than the
#' spurious correlation we had before (0.579), it still seems quite substantial.
#' But perhaps you can think of a confounder other than time that might influence both the
#' slopes in margarine consumption and the slopes in divorce rate.
#' 
#' 
#' We're not quite done yet, we can still answer more interesting questions with 
#' latent growth models. So far we've looked at whether state-level trends over time
#' are associated with one another, but what about the residuals within states 
#' around the trend? By this I mean, after the wider trends in margarine consumption
#' and divorce rates over time are removed from the variance in margarine consumption
#' and divorce rates, does an association between the two variables exist? 
#' 
#' A more real-world consequential example might be a question like: after removing
#' the general developmental trajectory that children are on in terms of their reading
#' skills and the complexity of the books they are reading (children generally tend to
#' develop their reading skills as they age and the complexity of the books they are
#' reading tends to increase as they age), do those children who are reading more complex
#' material tend to have higher reading skills (or does reading books that are too complex
#' actually reduce their reading comphrehension?). A host of latent growth models have been
#' developed (largely by Patrick Curran and colleagues) to assess these kinds of questions,
#' especially when looking at lagged effects. We'll start with a simple same-time point
#' association model though. I'll call it the paralell processes with correlated residuals
#' model though I'm sure there's a proper name for it:

divorce_margarine_pp_cr_mod <- "

  i_div =~ 1*divorce_1 + 1*divorce_2 + 1*divorce_3 + 1*divorce_4 + 
           1*divorce_5 + 1*divorce_6 + 1*divorce_7 + 1*divorce_8 + 
           1*divorce_9 + 1*divorce_10
  s_div =~ 0*divorce_1 + 1*divorce_2 + 2*divorce_3 + 3*divorce_4 + 
           4*divorce_5 + 5*divorce_6 + 6*divorce_7 + 7*divorce_8 + 
           8*divorce_9 + 9*divorce_10
  
  i_marg =~ 1*margarine_1 + 1*margarine_2 + 1*margarine_3 + 1*margarine_4 + 
           1*margarine_5 + 1*margarine_6 + 1*margarine_7 + 1*margarine_8 + 
           1*margarine_9 + 1*margarine_10
  s_marg =~ 0*margarine_1 + 1*margarine_2 + 2*margarine_3 + 3*margarine_4 + 
           4*margarine_5 + 5*margarine_6 + 6*margarine_7 + 7*margarine_8 + 
           8*margarine_9 + 9*margarine_10
  
  # Estimate covariance between all latent variables
  i_div ~~ s_div + i_marg + s_marg
  s_div ~~ i_marg + s_marg
  i_marg ~~ s_marg
  
  
  # Fix all of the residual variance for the indicator
  # variables to be the same (maybe too strong an assumption
  # to make, but can be tested)
  divorce_1 ~~ resd1*divorce_1
  divorce_2 ~~ resd1*divorce_2
  divorce_3 ~~ resd1*divorce_3
  divorce_4 ~~ resd1*divorce_4
  divorce_5 ~~ resd1*divorce_5
  divorce_6 ~~ resd1*divorce_6
  divorce_7 ~~ resd1*divorce_7
  divorce_8 ~~ resd1*divorce_8
  divorce_9 ~~ resd1*divorce_9
  divorce_10 ~~ resd1*divorce_10
  
  margarine_1 ~~ resd2*margarine_1
  margarine_2 ~~ resd2*margarine_2
  margarine_3 ~~ resd2*margarine_3
  margarine_4 ~~ resd2*margarine_4
  margarine_5 ~~ resd2*margarine_5
  margarine_6 ~~ resd2*margarine_6
  margarine_7 ~~ resd2*margarine_7
  margarine_8 ~~ resd2*margarine_8
  margarine_9 ~~ resd2*margarine_9
  margarine_10 ~~ resd2*margarine_10
  
  # Estimate the covariance between margarine consumption and divorce rate
  # residuals at the same time point.
  # We label all of these correlations to be fixed to be the same by using cr*
  # By fixing the residual variance above this means they will all have the same
  # standardised value too. Again, the assumption that these should all be the
  # same at each time point can be empirically tested by comparing the model fit
  # between the more constained model (this one) and one where the correlations
  # are freely estimated (a different relationship within each time point)
  
  divorce_1 ~~ cr*margarine_1
  divorce_2 ~~ cr*margarine_2
  divorce_3 ~~ cr*margarine_3
  divorce_4 ~~ cr*margarine_4
  divorce_5 ~~ cr*margarine_5
  divorce_6 ~~ cr*margarine_6
  divorce_7 ~~ cr*margarine_7
  divorce_8 ~~ cr*margarine_8
  divorce_9 ~~ cr*margarine_9
  divorce_10 ~~ cr*margarine_10

"

ppcrmod_out <- growth(divorce_margarine_pp_cr_mod, data = div_dat)
summary(ppcrmod_out, standardized = TRUE)

#' We're now up to some quite complicated models but hopefully you are 
#' following along! There's no point only showing very simple models that
#' are of little practical use in real research. 
#' 
#' We are now making some much stronger assumptions about the variance
#' in our indicator variables. These kinds of assumptions are made by
#' default in multilevel modelling frameworks, so their imposition is 
#' not unjustifiable, but you can see that they change some of our 
#' model results quite substantially (our correlation between margarine
#' slopes and divorce slopes has now increased to 0.476!). This goes to
#' show you how big of an impact some of the assumptions we implicitly 
#' accept in our modelling frameworks can make - even though they may
#' be justified, the idea that residual variance at each measurement is 
#' equal across all time points should probably be tested!
#' 
#' It should be fairly obvious what our new parameters are: the whole host
#' of repeated (because we have set them to be the same) cr covariance 
#' parameters between divorce and margarine consumption at each time point.
#' 
#' The correlation between the residuals of -0.105 suggests that, on average,
#' there is a very small negative association between margarine consumption 
#' and divorce rates after wider trends have been adjusted for. The coefficient
#' suggests that states with margarine consumption residuals one standard 
#' deviation above what the trend would predict had, on average, divorce rates
#' 0.105 standard deviations **below** what the trend would suggest. This 
#' adds complexity to our story: despite general trends being positively correlated,
#' the association within the trend is negatively correlated (perhaps the general
#' trend reflects an aversion to trans fats in general and fewer unhappy marriages,
#' but the relationship within the residuals suggests more health-conscious behaviours
#' to pick margarine over butter, which might lead to happier marriages...).
#' 
#' As you can see, it's not only the structure but the interpretation of these models
#' that becomes quite complex. In my research I am interested in separating the wider
#' trends of spending cuts as a result of national austerity policies and their association
#' with the rates of children in the care system from what happens within a local authority
#' when they spent more or less in a given year than their trend would suggest: do their
#' rates of children in care go up or down more than the general trend would suggest in
#' the following year? 
#' 


# Closing thoughts and further resources ----------------------------------

#' Structural equation models are a powerful, flexible, set of tools. As with anything
#' that fits this description, they require a better degree of understanding than most 
#' tools to use effectively. 
#' 
#' I would strongly recommend the training courses run by Dr. Chris Stride (CStat):
#' http://figureitout.org.uk/ I am not affiliated with Chris' 
#' training and consultancy. I found them to be the best source 
#' of training and material by far when I was a PhD student learning how to do SEM. 
#' 
#' Unfortunately, there is a bit of a dearth of accessible textbooks for learning 
#' structural equation modelling. Many of the best things you can do to learn how to
#' use it effectively involve making sure you have a very strong foundational knowledge
#' of regression and statistical concepts like variance, covariance, etc. Some books I 
#' could recommend in order of preference are:
#' 
#' For SEM and latent variable modelling:
#' * Latent Variable Modeling Using R: A Step-by-Step Guide by A. A. Beaujean
#' * Using Multivariate Statistics by Tabachnik & Fidell
#' * The lavaan tutorial: https://lavaan.ugent.be/tutorial/index.html 
#' 
#' For latent growth modelling:
#' * Longitudinal Structural Equation Modeling by Todd D. Little
#' * Growth Modeling: Structural Equation and Multilevel Modeling Approaches by Grimm, Ram & Estabrook
#' 
#' The reference manual for Mplus and discussions on the Mplus user forums 
#' can also be massively instructive, though you will need some knowledge to 
#' convert between Mplus and lavaan code. There are many features in Mplus that
#' lavaan is not yet capable of, but that list is growing shorter every year.
#' 


