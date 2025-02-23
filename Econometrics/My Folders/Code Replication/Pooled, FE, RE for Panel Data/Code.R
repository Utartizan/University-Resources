# ============================================================================= #
# Part 1: Setting up Packages/Libraries for Panel Data Methods
# ============================================================================= #

# Set seed for reproducibility
set.seed(123)

install.packages(c("AER", "plm", "tidyverse"))

# Load required packages
library("AER")        # For Grunfeld dataset and econometric tools
library("plm")        # For panel data regression
library("tidyverse")  # For data manipulation






# ============================================================================= #
# Part 2: Grunfeld Investment Data - Panel Data Basics
# ============================================================================= #

# Load and inspect the Grunfeld dataset (balanced panel: firms over years)
data("Grunfeld", package = "AER")
head(Grunfeld)  # Check structure: firm, year, invest, value, capital

# 1. Pooled OLS (treating data as cross-sectional)
pooled_ols <- lm(invest ~ value + capital, data = Grunfeld)
summary(pooled_ols)

# 2. Fixed Effects (FE) - Individual (firm) effects
fe_firm <- plm(invest ~ value + capital, data = Grunfeld, 
               index = c("firm", "year"), model = "within")
summary(fe_firm)

# Verify with manual FE using lm()
fe_firm_manual <- lm(invest ~ value + capital + as.factor(firm), data = Grunfeld)
summary(fe_firm_manual)  # Coefficients for value and capital should match fe_firm

# 3. Two-way FE - Individual (firm) and time (year) effects
fe_twoways <- plm(invest ~ value + capital, data = Grunfeld, 
                  index = c("firm", "year"), model = "within", effect = "twoways")
summary(fe_twoways)

# 4. Random Effects (RE)
re_model <- plm(invest ~ value + capital, data = Grunfeld, 
                index = c("firm", "year"), model = "random")
summary(re_model)

# Hausman Test: Compare FE vs RE (null: RE is consistent and efficient)
phtest(fe_firm, re_model)

# Key takeaway for assessment: 
# - Pooled OLS ignores panel structure
# - FE controls for unobserved heterogeneity (e.g., firm-specific effects)
# - RE assumes unobserved effects are uncorrelated with regressors
# - Hausman test helps choose between FE and RE




# ============================================================================= #
# Part 3: Simulated CAPM - Panel Regression with Interaction Terms
# ============================================================================= #

# Simulate simple CAPM-like data (since we can’t fetch Yahoo Finance data here)
n_months <- 24
tickers <- c("StockA", "StockB")
sim_data <- data.frame(
  Date = rep(seq(as.Date("2020-01-01"), by = "month", length.out = n_months), times = length(tickers)),
  Ticker = rep(tickers, each = n_months),
  Excess_Return = rnorm(n_months * length(tickers), mean = 0.5, sd = 2),  # Simulated stock returns
  Market_Return = rep(rnorm(n_months, mean = 0.3, sd = 1), times = length(tickers))  # Simulated market returns
)

# 1. Pooled regression (ignoring ticker differences)
capm_pooled <- lm(Excess_Return ~ Market_Return, data = sim_data)
summary(capm_pooled)

# 2. FE with ticker-specific intercepts
capm_fe <- lm(Excess_Return ~ Market_Return + as.factor(Ticker), data = sim_data)
summary(capm_fe)

# 3. FE with ticker-specific slopes (interaction terms)
capm_interaction <- lm(Excess_Return ~ Market_Return + Market_Return:as.factor(Ticker), data = sim_data)
summary(capm_interaction)

# Key takeaway for assessment:
# - Interaction terms allow slopes (e.g.,CAPM beta) to vary by group (ticker)
# - Compare R-squared and coefficients to assess fit

# ============================================================================= #
# Part 4: Policy Analysis - Guns and Violent Crime (Simplified)
# ============================================================================= #

# Load Guns dataset
data("Guns")
head(Guns)  # Check structure: state, year, violent, law

# 1. Pooled OLS
guns_pooled <- plm(log(violent) ~ law, data = Guns, 
                   index = c("state", "year"), model = "pooling")
summary(guns_pooled)

# 2. Fixed Effects (state-specific)
guns_fe <- plm(log(violent) ~ law, data = Guns, 
               index = c("state", "year"), model = "within")
summary(guns_fe)

# 3. Two-way FE (state and year)
guns_twoways <- plm(log(violent) ~ law, data = Guns, 
                    index = c("state", "year"), model = "within", effect = "twoways")
summary(guns_twoways)

# Key takeaway for assessment:
# - Policy variable (law) effect may differ across models
# - Two-way FE controls for both spatial (state) and temporal (year) unobserved effects

# ============================================================================= #
# Tips for Your Assessment
# ============================================================================= #
# - Understand the difference between pooled OLS, FE, and RE
# - Be ready to interpret coefficients and significance
# - Know when to use Hausman test and what it tests
# - Practice explaining why two-way FE might be preferred in policy analysis