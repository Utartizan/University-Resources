# Install required packages
#install.packages(c("tidyverse", "stats", "margins"))


# Load required packages
library(tidyverse)  # For data manipulation and plotting
library(stats)      # For glm() - logit and probit models

# Prepare the mtcars dataset
data(mtcars)
mtcars <- mtcars %>% 
  mutate(am_binary = as.factor(am))  # Binary outcome: 0 = auto, 1 = manual

# 1. Logit Model (Logistic Regression)
logit_model <- glm(am_binary ~ hp + wt, 
                   data = mtcars, 
                   family = binomial(link = "logit"))
summary(logit_model)

# 2. Probit Model
probit_model <- glm(am_binary ~ hp + wt, 
                    data = mtcars, 
                    family = binomial(link = "probit"))
summary(probit_model)

# 3. Predicted Probabilities
mtcars$logit_prob <- predict(logit_model, type = "response")  # Probabilities
mtcars$probit_prob <- predict(probit_model, type = "response")

# 4. Plot Logit vs. Probit Predictions
ggplot(mtcars, aes(x = hp)) +
  geom_point(aes(y = am, color = "Actual"), alpha = 0.5) +
  geom_line(aes(y = logit_prob, color = "Logit"), size = 1) +
  geom_line(aes(y = probit_prob, color = "Probit"), size = 1, linetype = "dashed") +
  labs(title = "Logit vs. Probit: Probability of Manual Transmission",
       x = "Horsepower (hp)", y = "Probability") +
  scale_color_manual(values = c("Actual" = "black", "Logit" = "blue", "Probit" = "red")) +
  theme_minimal()

# 5. Marginal Effects (for Logit) - approximate change in probability
library(margins)  # For marginal effects
logit_margins <- margins(logit_model)
summary(logit_margins)