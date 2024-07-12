library(lme4)
library(ggplot2)
library(performance)
library(DHARMa)
library(lmerTest)
library(tidyverse)
library(emmeans)
library(gridExtra)


# Dataframe for learning (Accuracy over Time)
data_learning <- data %>%
  group_by(Player_ID, Question_Num) %>%
  summarise(Average_Accuracy = mean(Answer), .groups = 'drop')

# Dataframe for hardest condition
data_condition <- data %>%
  group_by(Player_ID, Phrase_Condition) %>%
  summarise(Average_Accuracy = mean(Answer), .groups = 'drop')

# Dataframe for the impact of different versions
data_version <- data %>%
  group_by(Player_ID, Game_Version) %>%
  summarise(Average_Accuracy = mean(Answer), .groups = 'drop')

# Dataframe for rate of improvement depending on the version and condition
data_improvement <- data %>%
  group_by(Player_ID, Question_Num, Phrase_Condition, Game_Version) %>%
  summarise(Average_Accuracy = mean(Answer), .groups = 'drop')

## What is the average accuracy marginalizing over everything?
### Ob: ac ~ 1 + (1 | participant)


# Dataframe for overall average accuracy per participant
data_ac <- data %>%
  group_by(Player_ID) %>%
  summarise(Average_Accuracy = mean(Answer), .groups = 'drop')

# Fit the model
model2 <- lm(Average_Accuracy ~ 1, data = data_ac)

# Obtain and print a summary of the model to examine fixed and random effects
print(summary(model2))

#Diagnostic plots
#plot_residuals_diagnostics(model2)
plot(model2)

# Diagnostic checks - Simulating residuals to validate model assumptions
residuals_simulation <- simulateResiduals(fittedModel = model2, n = 500)
plot(residuals_simulation)

# Model diagnostics
check_model(model2)

# If assumptions are not met, consider re-fitting the model with transformations or different specifications
# model_revised <- update(model, . ~ . + log(Reaction_Time))
# print(summary(model_revised))


## How does the effect of condition vary depending on the version?
### 4b: ac - 1 + condition * version + (1 + condition | participant)


# Dataframe for condition and version interaction
data_condition_version <- data %>%
  group_by(Player_ID, Phrase_Condition, Game_Version) %>%
  summarise(Average_Accuracy = mean(Answer), .groups = 'drop')

#This doesnt work:
#model4 <- lmer(Average_Accuracy ~ 1 + Phrase_Condition * Game_Version + (1 + Phrase_Condition | Player_ID), data = data_condition_version)
model4 <- lmer(Average_Accuracy ~ 1 + Phrase_Condition * Game_Version + (1 | Player_ID), data = data_condition_version)

# Obtain and print a summary of the model to examine fixed and random effects
print(summary(model4))

#Diagnostic plots
plot_residuals_diagnostics(model4)

# Diagnostic checks - Simulating residuals to validate model assumptions
residuals_simulation <- simulateResiduals(fittedModel = model4, n = 500)
plot(residuals_simulation)

# Model diagnostics
check_model(model4)

# Check for influential cases potentially affecting the model
#influence_measures <- influence(model4)
#plot(influence_measures, which = "cook")

# If assumptions are not met, consider re-fitting the model with transformations or different specifications
# model_revised <- update(model, . ~ . + log(Reaction_Time))
# print(summary(model_revised))

# Model interpretation - examining fixed and random effects
fixed_effects <- fixef(model4)
random_effects <- ranef(model4)

# Printing fixed effects for interpretation
print(fixed_effects)

# Printing random effects for interpretation
print(random_effects)

# Conducting pairwise comparisons of phrase conditions using estimated marginal means
emm_results <- emmeans(model4, specs = pairwise ~ Phrase_Condition)
print(summary(emm_results))
