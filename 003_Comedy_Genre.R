## Pull Genre: Comedy --------
comedy_df <- dat %>%
  mutate(genre1 = ifelse(
    genre2 == "Comedy",
    genre2,
    ifelse(
      genre3 == "Comedy",
      genre3,
      genre1
    )
  )) %>%
  filter(genre1 == "Comedy") %>%
  select(
    tconst, # ID: constant value
    primaryTitle, # ID: proper name, might drop
    startYear, # year variable (DV)
    runtimeMinutes, # covar?
    genre1, # selection var
    averageRating, # outcome
    numVotes # weight
  ) %>%
  filter(!is.na(startYear)) %>%
  filter(!is.na(averageRating)) %>%
  filter(!is.na(runtimeMinutes)) %>%
  filter(runtimeMinutes > 0) # One film had a runtime of 0

## Correlation tests ------
cor.test(comedy_df$startYear, comedy_df$averageRating)
cor.test(comedy_df$averageRating, comedy_df$runtimeMinutes)
cor.test(comedy_df$runtimeMinutes, comedy_df$startYear)
cor.test(comedy_df$averageRating, comedy_df$numVotes)

## Year x Average Rating: Pearson's r = 0.1224866, p < .05
## Average Rating x Runtimes: Pearson's r = -0.3319822, p < .05
## Runtime x Start Year: Pearson's r = -.003276958, p < .05
## Average Rating x Number of Votes: Pearson's r = 0.01786279, r <.05

## Modeling ---------
library(lavaan)

model <- '
  # direct effect
  averageRating ~ c*startYear
  
  # mediation
  runtimeMinutes ~ a*startYear
  averageRating ~ b*runtimeMinutes
  
  # indirect effect
  ab := a *b
  
  # total effect
  total := c + (a*b)
'

fit <- sem(model, data = action_df,
           se = "bootstrap",
           bootstrap = 1000)
summary(fit, fit.measures = TRUE, 
        standardized = TRUE)
