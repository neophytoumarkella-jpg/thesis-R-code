# BACHELOR THESIS — DATA ANALYSIS SCRIPT

# STEP 1: LOAD PACKAGES

library(readr)
library(dplyr)
library(tidyr)
library(psych)
library(ggplot2)
library(car)
library(lmtest)
library(interactions)
library(mediation)
library(pwr)
library(table1)

# STEP 2: LOAD DATA
library(readxl)
Response_Data_copy <- read_excel("Response Data copy.xlsx")
View(Response_Data_copy)

# Raw data: 
nrow(Response_Data_copy) #[1] 514
cat("Raw data:", nrow(Response_Data_copy), "\n")

# STEP 3: DATA CLEANING

# Remove header row
Clean_data <- Response_Data_copy[-1, ]
cat("After header row removed:", nrow(Clean_data), "\n")

# Remove survey previews
Clean_data <- Clean_data[Clean_data$Status != "Survey Preview", ]
cat("After previews removed:", nrow(Clean_data), "\n")

# Remove non-consenters
Clean_data <- Clean_data[grepl("Yes, I give consent", Clean_data$Q92), ]
cat("After non-consenters removed:", nrow(Clean_data), "\n")

# Remove attention check failures
Clean_data <- Clean_data[Clean_data$`Attention Check` == "green", ]  #N=393
cat("After attention-check fails removed:", nrow(Clean_data), "\n")

# Keep females only
Clean_data <- Clean_data[!is.na(Clean_data$`Gender Identity`) & 
                           Clean_data$`Gender Identity` == "Female", ]
nrow(Clean_data) # N = 190
cat("After non-females removed:", nrow(Clean_data), "\n")   # N = 190



# STEP 4: RECODE SCALES (text to numeric)
# Political ideology (1 = Strongly liberal, 7 = Strongly conservative)
Clean_data$Q28_1 <- case_when(
  Clean_data$Q28_1 == "Strongly liberal"                ~ 1,
  Clean_data$Q28_1 == "Liberal"                         ~ 2,
  Clean_data$Q28_1 == "Somewhat liberal"                ~ 3,
  Clean_data$Q28_1 == "Neither liberal nor conservative" ~ 4,
  Clean_data$Q28_1 == "Somewhat conservative"           ~ 5,
  Clean_data$Q28_1 == "Conservative"                    ~ 6,
  Clean_data$Q28_1 == "Strongly Conservative"           ~ 7
)

# Gender Role Beliefs (GRBS-S, 1-7)
Clean_data <- mutate(Clean_data, across(starts_with("GRBS-S"), ~ case_when(
  . == "Strongly disagree" ~ 1,
  . == "Slightly disagree" ~ 2,
  . == "Disagree"          ~ 3,
  . == "Undecided"         ~ 4,
  . == "Slightly agree"    ~ 5,
  . == "Agree"             ~ 6,
  . == "Strongly Agree"    ~ 7
)))

# System Justification (SJS, 1-7)
Clean_data <- mutate(Clean_data, across(matches("SJS"), ~ case_when(
  . == "Strongly disagree"          ~ 1,
  . == "Somewhat disagree"          ~ 2,
  . == "Disagree"                   ~ 3,
  . == "Neither agree nor disagree" ~ 4,
  . == "Somewhat agree"             ~ 5,
  . == "Agree"                      ~ 6,
  . == "Strongly agree"             ~ 7
)))

# Life Satisfaction (SWLS, 1-7)
Clean_data <- mutate(Clean_data, across(starts_with("Life Satisfaction"), ~ case_when(
  . == "Strongly disagree"        ~ 1,
  . == "Somewhat disagree"        ~ 2,
  . == "Disagree"                 ~ 3,
  . == "Neither disagree or agree" ~ 4,
  . == "Somewhat agree"           ~ 5,
  . == "Agree"                    ~ 6,
  . == "Strongly agree"           ~ 7
)))

# Positive Affect (PANAS PA subscale, 1-5)
Clean_data <- mutate(Clean_data, across(starts_with("PA Scale"), ~ case_when(
  . == "Very slightly or not at all" ~ 1,
  . == "A little"                    ~ 2,
  . == "Moderately"                  ~ 3,
  . == "Quite a bit"                 ~ 4,
  . == "Extremely"                   ~ 5
)))

# STEP 5: REVERSE CODING

# GRBS-S item 3: "Women should have as much sexual freedom as men" (reverse)
Clean_data$`GRBS-S_3` <- 8 - Clean_data$`GRBS-S_3`

# SJS items 3 and 6 (reverse)
sjs_names <- colnames(Clean_data)[grepl("SJS", colnames(Clean_data))]
Clean_data[[sjs_names[3]]] <- 8 - Clean_data[[sjs_names[3]]]
Clean_data[[sjs_names[6]]] <- 8 - Clean_data[[sjs_names[6]]]


# STEP 6: COMPUTE COMPOSITE SCORES

Clean_data$political_ideology  <- Clean_data$Q28_1
Clean_data$gender_role_beliefs <- rowMeans(Clean_data[, paste0("GRBS-S_", 1:10)], na.rm = TRUE)
Clean_data$system_justification <- rowMeans(Clean_data[, sjs_names], na.rm = TRUE)
Clean_data$life_satisfaction   <- rowMeans(Clean_data[, paste0("Life Satisfaction_", 1:5)], na.rm = TRUE)
Clean_data$positive_affect     <- rowMeans(Clean_data[, paste0("PA Scale_", 1:10)], na.rm = TRUE)

# Standardize life satisfaction and positive affect, then combine into SWB
Clean_data$life_sat_z <- as.numeric(scale(Clean_data$life_satisfaction))
Clean_data$pa_z       <- as.numeric(scale(Clean_data$positive_affect))
Clean_data$SWB        <- rowMeans(cbind(Clean_data$life_sat_z, Clean_data$pa_z), na.rm = TRUE)


# STEP 7: CONVERT VARIABLE TYPES
Clean_data$Age                  <- as.numeric(Clean_data$Age)
Clean_data$`Gender Identity`    <- as.factor(Clean_data$`Gender Identity`)
Clean_data$Education            <- as.factor(Clean_data$Education)
Clean_data$`Country of Residence` <- as.factor(Clean_data$`Country of Residence`)
Clean_data$political_ideology   <- as.numeric(Clean_data$political_ideology)
Clean_data$gender_role_beliefs  <- as.numeric(Clean_data$gender_role_beliefs)
Clean_data$system_justification <- as.numeric(Clean_data$system_justification)

# Clean impossible ages
Clean_data$Age <- ifelse(Clean_data$Age > 100 | Clean_data$Age < 18, NA, Clean_data$Age)
cat("Number with impossible age set to NA:", sum(is.na(Clean_data$Age)), "\n")



# STEP 8: SAMPLE DESCRIPTIVES

# Age
mean(Clean_data$Age, na.rm = TRUE)   # M = 31.39
sd(Clean_data$Age, na.rm = TRUE)     # SD = 13.72
range(Clean_data$Age, na.rm = TRUE)  # 18-79

# Country and birthplace
table(Clean_data$`Country of Residence`)
table(Clean_data$Birthplace)
length(unique(Clean_data$`Country of Residence`)) # 14 countries
length(unique(Clean_data$Birthplace))              # 27 birthplaces

table(Clean_data$Birthplace)

# Questionnaire duration

median(as.numeric(Clean_data$`Duration (in seconds)`), na.rm = TRUE) / 60 # ~24 minutes

# STEP 9: POWER ANALYSIS

pwr.f2.test(u = 3, f2 = 0.15, sig.level = 0.05, power = 0.80)

# Minimum N = 77; analytic sample N = 186 — sufficiently powered


# STEP 10: RELIABILITY ANALYSES

# Gender Role Beliefs (GRBS-S, 10 items)
psych::alpha(Clean_data[, paste0("GRBS-S_", 1:10)])
# α = 0.82

# Life Satisfaction (SWLS, 5 items)
psych::alpha(Clean_data[, paste0("Life Satisfaction_", 1:5)])
# α = 0.83

# System Justification (SJS, 7 items)
sjs_cols <- colnames(Clean_data)[grepl("SJS", colnames(Clean_data))]
psych::alpha(Clean_data[, sjs_cols], check.keys = TRUE)
# α = 0.72

# Positive Affect (PANAS PA subscale, 10 items)
psych::alpha(Clean_data[, paste0("PA Scale_", 1:10)])
# α = 0.86


# STEP 11: CREATE ANALYTIC DATASET (complete cases)=> to see if everyone completed my scales
cat("Before completeness check:", nrow(Clean_data), "\n")  

Clean_data_med <- Clean_data[complete.cases(Clean_data[, c(
  "political_ideology", "gender_role_beliefs", "system_justification",
  "SWB", "Age", "Education")]), ]

nrow(Clean_data_med) # N = 186

nrow(Clean_data) - nrow(Clean_data_med) 

cat("After completeness check:", nrow(Clean_data_med), "\n")
cat("Total excluded for incomplete data:", nrow(Clean_data) - nrow(Clean_data_med), "\n")  

# Break down: which variables had missing data in the 4 excluded participants?
excluded <- Clean_data[!complete.cases(Clean_data[, c(
  "political_ideology", "gender_role_beliefs", "system_justification",
  "SWB", "Age", "Education")]), ]

cat("\nOf the 4 excluded, missing data breakdown:\n")
cat("Missing political_ideology:", sum(is.na(excluded$political_ideology)), "\n")
cat("Missing gender_role_beliefs:", sum(is.na(excluded$gender_role_beliefs)), "\n")
cat("Missing system_justification:", sum(is.na(excluded$system_justification)), "\n")
cat("Missing SWB:", sum(is.na(excluded$SWB)), "\n")
cat("Missing Age:", sum(is.na(excluded$Age)), "\n")
cat("Missing Education:", sum(is.na(excluded$Education)), "\n")






# Group education ( cells with only a few participants are collapsed into 'Secondary or Below')
Clean_data_med$Education_grouped <- as.character(Clean_data_med$Education)
Clean_data_med$Education_grouped[Clean_data_med$Education_grouped %in% 
                                   c("Secondary", "Some Secondary", "Vocational or Similar", "Prefer not to say")] <- "Secondary or Below"
# Make education a factor
Clean_data_med$Education_grouped <- as.factor(Clean_data_med$Education_grouped)

# Check groupings
table(Clean_data_med$Education_grouped)


#country of residence and birthplace tables for descriptives
table(Clean_data_med$country_of_residence) |> sort(decreasing = TRUE)
table(Clean_data_med$Birthplace) |> sort(decreasing = TRUE)

# Country and birthplace after all exclusion criteria are applied
table(Clean_data_med$`Country of Residence`)
table(Clean_data_med$Birthplace)
length(unique(Clean_data_med$`Country of Residence`)) # 14 countries
length(unique(Clean_data_med$Birthplace))              # 27 birthplaces


#scatterplots 
plot(Clean_data_med$political_ideology, Clean_data_med$gender_role_beliefs,
     xlab = "Political Ideology", ylab = "Gender Role Beliefs")
abline(lm(gender_role_beliefs ~ political_ideology, data = Clean_data_med), col = "pink")

plot(Clean_data_med$political_ideology, Clean_data_med$system_justification,
     xlab = "Political Ideology", ylab = "System Justification")
abline(lm(system_justification ~ political_ideology, data = Clean_data_med), col = "pink")

plot(Clean_data_med$political_ideology, Clean_data_med$SWB,
     xlab = "Political Ideology", ylab = "Subjective Wellbeing")
abline(lm(SWB ~ political_ideology, data = Clean_data_med), col = "pink")

plot(Clean_data_med$system_justification, Clean_data_med$SWB,
     xlab = "System Justification", ylab = "Subjective Wellbeing")
abline(lm(SWB ~ system_justification, data = Clean_data_med), col = "pink")



# STEP 12: DESCRIPTIVE STATISTICS

mean(Clean_data_med$political_ideology, na.rm = TRUE)   # M = 2.48
sd(Clean_data_med$political_ideology, na.rm = TRUE)     # SD = 1.32
mean(Clean_data_med$gender_role_beliefs, na.rm = TRUE)  # M = 2.42
sd(Clean_data_med$gender_role_beliefs, na.rm = TRUE)    # SD = 0.93
mean(Clean_data_med$system_justification, na.rm = TRUE) # M = 2.97
sd(Clean_data_med$system_justification, na.rm = TRUE)   # SD = 0.90
mean(Clean_data_med$SWB, na.rm = TRUE)                  # M = -0.01
sd(Clean_data_med$SWB, na.rm = TRUE)                    # SD = 0.87

#TABLE 1: 

# Descriptive table
label(Clean_data_med$Age)               <- "Age (years)"
label(Clean_data_med$Education_grouped) <- "Education"
label(Clean_data_med$`Country of Residence`) <- "Country of Residence"

table1(~ Age + Education_grouped + `Country of Residence`, data = Clean_data_med)


# STEP 13: CORRELATIONS

corr.test(Clean_data_med[, c("political_ideology", "gender_role_beliefs",
                             "system_justification", "SWB")],
          use = "pairwise.complete.obs")
# PI-GRB: r = .52***
# PI-SJT: r = .35***
# PI-SWB: r = .23***
# GRB-SJT: r = .25***
# GRB-SWB: r = .14, p = .06 (marginal)
# SJT-SWB: r = .28***

corr.test(Clean_data_med[, c("political_ideology", "gender_role_beliefs",
                             "system_justification", "SWB")],
          use = "pairwise.complete.obs",)

#To get the exact p values instead of P <.001
corr.test(Clean_data_med[, c("political_ideology", "gender_role_beliefs",
                             "system_justification", "SWB")],
          use = "pairwise.complete.obs")$p

#Degrees of freedom 
#df= N-2= 184
nrow(Clean_data_med) - 2


# STEP 14: REGRESSION MODELS (unstandardized)

model_a1 <- lm(gender_role_beliefs ~ political_ideology + Age + 
                 Education_grouped, data = Clean_data_med)

model_a2 <- lm(system_justification ~ political_ideology + Age + 
                 Education_grouped, data = Clean_data_med)

model_b  <- lm(SWB ~ political_ideology + gender_role_beliefs + 
                 system_justification + Age + Education_grouped, data = Clean_data_med)

summary(model_a1)

summary(model_a2)

summary(model_b)







# STEP 15: REGRESSION MODELS (standardized, for β coefficients)

install.packages("lm.beta")
library(lm.beta)

model_a1_z <- lm.beta(model_a1)
model_a2_z <- lm.beta(model_a2)
model_b_z <- lm.beta(model_b)

summary(model_a1_z)
summary(model_a2_z)
summary(model_b_z)




# STEP 16: ASSUMPTION CHECKS

# Normality of residuals (Q-Q plots)
plot(model_a1, which = 2)
plot(model_a2, which = 2)
plot(model_b,  which = 2)
# Minor deviations in lower tail, acceptable

# Homoscedasticity (Scale-Location plots)
plot(model_a1, which = 3)
plot(model_a2, which = 3)
plot(model_b,  which = 3)
# Mild heteroscedasticity in A1 and B, acceptable given bootstrapping

# Influential outliers (Cook's distance)
plot(model_a1, which = 4)
plot(model_a2, which = 4)
plot(model_b,  which = 4)
# All Cook's D < 0.07, no influential outliers

# Linearity (Residuals vs Fitted)
plot(model_a1, which = 1)
plot(model_a2, which = 1)
plot(model_b,  which = 1)
# Red line approximately flat, linearity met

# Multicollinearity (VIF)
vif(model_a1)
vif(model_a2)
vif(model_b)
# All VIF < 2, no multicollinearity concerns

# STEP 17: MEDIATION ANALYSES

# GRB as mediator (H6)
mediation_GRB <- mediate(model_a1, model_b,
                         treat    = "political_ideology",
                         mediator = "gender_role_beliefs",
                         boot = TRUE, sims = 5000)
summary(mediation_GRB)
# ACME = 0.002, 95% CI [-0.067, 0.072], p = .963 — NOT supported

# SJT as mediator (H7)
mediation_SJT <- mediate(model_a2, model_b,
                         treat    = "political_ideology",
                         mediator = "system_justification",
                         boot = TRUE, sims = 5000)
summary(mediation_SJT)
# ACME = 0.041, 95% CI [0.006, 0.090], p = .018 — SUPPORTED
# ADE  = 0.087, p = .150 (ns) → full mediation
# Total effect = 0.128, p = .038
# Proportion mediated = 32%


# For regression table: extract B, SE, β, p from summaries
# Model A1 PI: B = 0.377, SE = 0.045, β = 0.537, p < .001
# Model A2 PI: B = 0.242, SE = 0.047, β = 0.354, p < .001
# Model B SJT: B = 0.169, SE = 0.078, β = 0.174, p = .032

# Model A1: F(10, 175) = 10.01, p < .001, R² = .36, adj R² = .33
# Model A2: F(10, 175) = 5.85, p < .001, R² = .25, adj R² = .21
# Model B:  F(12, 173) = 2.73, p = .002, R² = .16, adj R² = .10


# For GRB mediation
mediation_GRB$d0.sims |> sd()

# For SJT mediation
mediation_SJT$d0.sims |> sd()
summary(mediation_SJT)


#BAYES FACTOR 
install.packages("BayesFactor")
library(BayesFactor)

correlationBF(Clean_data_med$political_ideology, Clean_data_med$gender_role_beliefs)
correlationBF(Clean_data_med$political_ideology, Clean_data_med$system_justification)
correlationBF(Clean_data_med$political_ideology, Clean_data_med$SWB)
correlationBF(Clean_data_med$gender_role_beliefs, Clean_data_med$system_justification)
correlationBF(Clean_data_med$gender_role_beliefs, Clean_data_med$SWB)
correlationBF(Clean_data_med$system_justification, Clean_data_med$SWB)



#REGRESSION MODELS — main predictors only 

## Model A1: PI → GRB

lmBF(gender_role_beliefs ~ political_ideology + Age + Education_grouped, 
     data = Clean_data_med)

# Model A2: PI → SJT

lmBF(system_justification ~ political_ideology + Age + Education_grouped, 
     data = Clean_data_med)

# Model B: full model

lmBF(SWB ~ political_ideology + gender_role_beliefs + system_justification + 
       Age + Education_grouped, data = Clean_data_med)

#COMPARISON model with and without bayes factor 

# Does PI add anything to GRB model?
full_a1    <- lmBF(gender_role_beliefs ~ political_ideology + Age + Education_grouped, data = Clean_data_med)
without_PI <- lmBF(gender_role_beliefs ~ Age + Education_grouped, data = Clean_data_med)
full_a1 / without_PI  # BF for PI in Model A1

# Does PI add anything to SJT model?
full_a2    <- lmBF(system_justification ~ political_ideology + Age + Education_grouped, data = Clean_data_med)
without_PI2 <- lmBF(system_justification ~ Age + Education_grouped, data = Clean_data_med)
full_a2 / without_PI2

# Does SJT add anything to Model B?
full_b     <- lmBF(SWB ~ political_ideology + gender_role_beliefs + system_justification + Age + Education_grouped, data = Clean_data_med)
without_SJT <- lmBF(SWB ~ political_ideology + gender_role_beliefs + Age + Education_grouped, data = Clean_data_med)
full_b / without_SJT

# Does GRB add anything to Model B?
without_GRB <- lmBF(SWB ~ political_ideology + system_justification + Age + Education_grouped, data = Clean_data_med)
full_b / without_GRB

# Does PI add anything to Model B (direct effect)?
without_PI_b <- lmBF(SWB ~ gender_role_beliefs + system_justification + Age + Education_grouped, data = Clean_data_med)
full_b / without_PI_b

#How to report:
#Bayesian model comparison indicated strong evidence for the inclusion of political ideology in predicting gender role beliefs, BF₁₀ = [value].
#Evidence for the inclusion of gender role beliefs in predicting SWB was anecdotal/weak, BF₁₀ = [value].

# MEDIATION INDIRECT EFFECTS (by testing the key paths)

# Path a1 × b1 (GRB mediation) — test b1 path specifically
without_GRB_b <- lmBF(SWB ~ political_ideology + system_justification + Age + Education_grouped, data = Clean_data_med)
full_b / without_GRB_b  # already done above

# Path a2 × b2 (SJT mediation) — test b2 path specifically
full_b / without_SJT  # already done above

#How to report:
#Bayesian analysis provided [strong/moderate/anecdotal] 
#evidence for the indirect path through system justification, BF₁₀ = [value], consistent with the bootstrapped mediation result.




#Anova type III for education
install.packages("car")
library(car)

# Set contrasts to sum-to-zero (REQUIRED for Type III ANOVA)
options(contrasts = c("contr.sum", "contr.poly"))

Anova(model_a1, type = "III")
Anova(model_a2, type = "III")
Anova(model_b,  type = "III")

options(contrasts = c("contr.treatment", "contr.poly"))



#Distributions in Histograms 
#SWB
ggplot(Clean_data_med, aes(x = SWB)) +
  geom_histogram(aes(y = ..density..), bins = 20, 
                 fill = "lightpink", color = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(Clean_data_med$SWB, na.rm = TRUE),
                            sd = sd(Clean_data_med$SWB, na.rm = TRUE)),
                color = "darkblue", size = 1) +
  labs(x = "Subjective Well-Being Score",
       y = "Density") +
  theme_classic(base_size = 12) +
  theme(plot.title = element_blank())  


# PI 
ggplot(Clean_data_med, aes(x = political_ideology)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, 
                 fill = "lightpink", color = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(Clean_data_med$political_ideology, na.rm = TRUE),
                            sd = sd(Clean_data_med$political_ideology, na.rm = TRUE)),
                color = "darkblue", size = 1) +
  labs(x = "Political Ideology Score",
       y = "Density") +
  theme_classic(base_size = 12) +
  theme(plot.title = element_blank())

#GRB
ggplot(Clean_data_med, aes(x = gender_role_beliefs)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, 
                 fill = "lightpink", color = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(Clean_data_med$gender_role_beliefs, na.rm = TRUE),
                            sd = sd(Clean_data_med$gender_role_beliefs, na.rm = TRUE)),
                color = "darkblue", size = 1) +
  labs(x = "Gender Role Beliefs Score",
       y = "Density") +
  theme_classic(base_size = 12) +
  theme(plot.title = element_blank())

#SJ

ggplot(Clean_data_med, aes(x = system_justification)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, 
                 fill = "lightpink", color = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(Clean_data_med$system_justification, na.rm = TRUE),
                            sd = sd(Clean_data_med$system_justification, na.rm = TRUE)),
                color = "darkblue", size = 1) +
  labs(x = "System Justification Score",
       y = "Density") +
  theme_classic(base_size = 12) +
  theme(plot.title = element_blank())


