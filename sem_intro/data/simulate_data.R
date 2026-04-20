
# Simulating a dataset for lecture on partial correlations.

# we will look at the relationship between three variables: age, sleep quality and reaction times.

#   Is there a direct relationship between sleep quality and reaction time,
#   *after removing the shared influence of age*?


# There is empirical evidence support the existence of these relationships:

#   - Older adults have both worse sleep quality (Ohayon et al., 2004, Sleep)
#     and slower reaction times (Salthouse, 1996, Psychological Review).
#   - Sleep deprivation impairs psychomotor vigilance / RT
#     (Lim & Dinges, 2010, Psychological Bulletin).
#   - Age is therefore a confounder: it inflates the zero-order
#     sleep–RT correlation beyond the direct sleep→RT effect.


# Here we create a simulated dataset that might look like real data.

# Variables:
#   age              (years; confounder)
#   sleep_quality    (1–10; higher = better; predictor of interest)
#   reaction_time_ms (ms; higher = slower; outcome of interest)


library(tidyverse)
library(MASS)

# -----------------------------------------------------------------------------
# 1. Simulate data with a known population correlation structure
# -----------------------------------------------------------------------------
# Population correlations (grounded in the literature above):
#   r(age, sleep_quality)    = -0.45  (older → worse sleep)
#   r(age, reaction_time_ms) =  0.55  (older → slower RT)
#   r(sleep_quality, RT)     = -0.35  (better sleep → faster RT, zero-order)
#
# Implied partial r(sleep, RT | age) ≈ -0.14  →  meaningful shrinkage
# -----------------------------------------------------------------------------

set.seed(123)
n <- 200

R_pop <- matrix(
  c( 1.00, -0.45,  0.55,
     -0.45,  1.00, -0.35,
     0.55, -0.35,  1.00),
  nrow = 3, byrow = TRUE,
  dimnames = list(c("age", "sleep_quality", "reaction_time"),
                  c("age", "sleep_quality", "reaction_time"))
)

# Generate standardised scores, then rescale to realistic units
raw <- MASS::mvrnorm(n = n, mu = rep(0, 3), Sigma = R_pop)

sleep_data <- as_tibble(raw) |>
  transmute(
    age              = round(age * 14 + 45),           # mean 45 yrs, SD 14
    sleep_quality    = round(sleep_quality * 1.5 + 6, 1), # mean 6/10, SD 1.5
    reaction_time    = round(reaction_time * 60 + 320)  # mean 320 ms, SD 60
  ) |>
  mutate(
    age              = pmax(18L, pmin(80L, age)),
    sleep_quality    = pmax(1,   pmin(10,  sleep_quality)),
    reaction_time    = pmax(150L, pmin(600L, reaction_time))/1000
  )


# save dataset
write_csv(sleep_data, 'sem_intro/data/sleep.csv')

