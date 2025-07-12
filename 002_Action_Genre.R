## Pull Genre: Action --------
action_primer <- dat %>% 
  mutate(genre1 =
           ifelse(
             genre2 == "Action",
             genre2,
             ifelse(
               genre3 == "Action",
               genre3,
               genre1
             )
           )) %>%
  filter(genre1 == "Action") %>%
  select(
    tconst, # ID: constant value
    primaryTitle, # ID: proper name, might drop
    startYear, # year variable (DV)
    runtimeMinutes, # covar?
    genre1, # selection var
    averageRating, # outcome
    numVotes # weight
  )

action_df <- action_primer %>%
  filter(!is.na(startYear)) %>%
  filter(!is.na(averageRating)) %>%
  filter(!is.na(runtimeMinutes))

### Correlation ---------
simple_cor <- action_primer %>%
  select(startYear, 
         averageRating,
         runtimeMinutes,
         numVotes) %>%
  filter(!is.na(startYear)) %>%
  filter(!is.na(averageRating)) %>%
  filter(!is.na(runtimeMinutes))

#### Correlation between year and rating: Pearson's R = .05647088, p < .05
cor.test(simple_cor$startYear, simple_cor$averageRating,)

### Correlation between runtimes and rating: Pearson's r = -0.3711001, p < .05
cor.test(simple_cor$runtimeMinutes, simple_cor$averageRating)

### Correlation between year and runtimes: Pearson's r = -0.1113734, p < .05
cor.test(simple_cor$startYear, simple_cor$runtimeMinutes)

### Correlation between rating and number of votes: Pearson's r = 0.03334554, p < .05
cor.test(simple_cor$numVotes, simple_cor$averageRating)

#### Visual -----------------
ggplot(action_df, aes(x = startYear,
                      y = averageRating)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Average Rating by Year",
    y = "IMDb Rating",
    x = "Year"
  )

ggplot(action_df, aes(x = startYear,
                      y = runtimeMinutes)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Runtimes by Year",
    y = "Runtimes",
    x = "Year"
  )

ggplot(action_df, aes(x = runtimeMinutes,
                      y = averageRating)) +
  geom_point(alpha = 0.3)+
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Average Ratings by Runtimes",
    x = "Runtimes",
    y = "IMDb Ratings"
  )

ggplot(action_df, aes(x = numVotes,
                      y = averageRating)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Average Rating by Number of Votes",
    x = "Number of Votes",
    y = "IMDb Ratings"
  )



#### Modeling! -----------
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

### Weighting by Num Votes ---------------
library(weights)

wtd.cor(action_df$startYear, action_df$averageRating, weight = action_df$numVotes)
# Weighted correlatin : cor == -0.05998733, p < .05

weight_lm <- lm(averageRating ~ startYear + runtimeMinutes,
                action_df,
                weights = action_df$numVotes)
summary(weight_lm)

fit_weighted <- sem(
  model,
  data = action_df,
  estimator = "ML",
  sampling.weights = "numVotes"
)

summary(fit_weighted, fit.measures = TRUE, standardized = TRUE)

fit_unweighted <- sem(
  model,
  data = action_df,
  se = "bootstrap",
  bootstrap = 1000
)

summary(fit_unweighted, fit.measures = TRUE, standardized = TRUE)

inspect(fit_weighted, "r2")
inspect(fit_unweighted, "r2")

fitMeasures(fit_weighted, c("cfi", "rmsea", "srmr"))
fitMeasures(fit_unweighted, c("cfi", "rmsea", "srmr"))

parameterEstimates(fit_unweighted, standardized = TRUE)
parameterEstimates(fit_weighted, standardized = TRUE)

