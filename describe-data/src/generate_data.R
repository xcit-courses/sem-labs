
# Generate an example dataset to cover different types of data.


library(dplyr)
set.seed(42)

n <- 100

# ── Names ──────────────────────────────────────────────────────────────────────
first_names <- c(
  "Alice", "Bob", "Carlos", "Diana", "Eva", "Frank", "Grace", "Henry",
  "Isabel", "James", "Karen", "Liam", "Mia", "Noah", "Olivia", "Paul",
  "Quinn", "Rachel", "Sam", "Tara", "Uma", "Victor", "Wendy", "Xavier",
  "Yara", "Zoe", "Ahmed", "Beatriz", "Chloe", "David", "Elena", "Felix",
  "Georgia", "Hiro", "Ingrid", "Jonas", "Keiko", "Lars", "Maya", "Nadia",
  "Oscar", "Priya", "Rafael", "Sofia", "Tariq", "Ursula", "Valentina",
  "Willem", "Xenia", "Yusuf"
)
last_names <- c(
  "Smith", "Johnson", "Williams", "Brown", "Jones", "Garcia", "Miller",
  "Davis", "Martinez", "Anderson", "Taylor", "Thomas", "Hernandez", "Moore",
  "Martin", "Jackson", "Thompson", "White", "Lopez", "Lee", "Gonzalez",
  "Harris", "Clark", "Lewis", "Robinson", "Walker", "Perez", "Hall", "Young",
  "Allen", "Sanchez", "Wright", "King", "Scott", "Green", "Baker", "Adams",
  "Nelson", "Carter", "Mitchell", "Pereira", "Nakamura", "Nielsen", "Müller",
  "Dubois", "Fernandez", "Costa", "Silva", "Rossi", "Ahmed"
)

participant_name <- paste(
  sample(first_names, n, replace = TRUE),
  sample(last_names,  n, replace = TRUE)
)

# ── Demographics ───────────────────────────────────────────────────────────────
sex         <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.49, 0.51))
age         <- round(rnorm(n, mean = 38, sd = 12))
age         <- pmax(18, pmin(80, age))          # clamp to 18–80
is_married  <- rbinom(n, 1, prob = 0.52) == 1
children_count <- ifelse(
  is_married,
  rpois(n, lambda = 1.8),
  rpois(n, lambda = 0.4)
)

# ── Physical measurements (sex-specific) ──────────────────────────────────────
height <- ifelse(
  sex == "Male",
  round(rnorm(n, mean = 176, sd = 7),  1),
  round(rnorm(n, mean = 163, sd = 6.5), 1)
)
# BMI ~ 18.5–30; weight derived from height
bmi    <- runif(n, min = 18.5, max = 30)
weight <- round(bmi * (height / 100)^2, 1)

# ── Cognitive & ratings ────────────────────────────────────────────────────────
IQ_score             <- round(rnorm(n, mean = 100, sd = 15))
IQ_score             <- pmax(60, pmin(145, IQ_score))
attractiveness_rating <- round(rnorm(n, mean = 5.5, sd = 1.5))
attractiveness_rating <- pmax(1, pmin(10, attractiveness_rating))

# ── Categorical variables ──────────────────────────────────────────────────────
countries <- c(
  "USA", "Germany", "France", "Brazil", "Japan",
  "UK",  "Canada",  "India",  "Mexico", "Australia"
)
country_probs <- c(0.22, 0.10, 0.09, 0.12, 0.10,
                   0.09, 0.08, 0.10, 0.06, 0.04)

country        <- sample(countries, n, replace = TRUE, prob = country_probs)
favorite_color <- sample(
  c("Blue", "Red", "Green", "Black", "White", "Purple", "Orange", "Yellow", "Pink"),
  n, replace = TRUE,
  prob = c(0.22, 0.15, 0.13, 0.12, 0.10, 0.09, 0.08, 0.06, 0.05)
)

# ── Assemble dataset ───────────────────────────────────────────────────────────
dat <- data.frame(
  participant_id        = sprintf("P%03d", seq_len(n)),
  participant_name      = participant_name,
  age                   = age,
  sex                   = sex,
  children_count        = children_count,
  is_married            = is_married,
  height                = height,
  weight                = weight,
  bmi                  = bmi,
  bmi_category         = cut(bmi, breaks = c(0, 18.5, 25, 30, Inf),
                             labels = c("Underweight", "Normal", "Overweight", "Obese")),
  IQ_score              = IQ_score,
  country               = country,
  favorite_color        = favorite_color,
  attractiveness_rating = attractiveness_rating,
  stringsAsFactors      = FALSE
)

# ── Save ───────────────────────────────────────────────────────────────────────
write.csv(dat, "describe-data/data.csv", row.names = FALSE)
# message("Saved ", nrow(dat), " rows to data.csv")
