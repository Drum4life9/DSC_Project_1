rm(list = ls())
setwd("~/Development/CSC_AT_LVC/DSC_340_Machine_learn/DSC_Project_1") # For Brian's mac
load("classroom6.RData")

data = classroom6

library(nlme)
library(dplyr)
library(knitr)
library(ggplot2)
library(lattice)
library(effects)


# Make factors easier to understand
data$sex <- factor(data$sex, 
                   levels = c(0, 1), 
                   labels = c("male", "female"))
data$minority <- factor(data$minority, 
                        levels = c(0, 1), 
                        labels = c("N", "Y"))



# Find the 33rd and 66th percentiles for teacher experience
cuts <- quantile(data$yearstea, probs = c(0.33, 0.66), na.rm = TRUE)

# Create the new factor
data <- data %>%
  mutate(tea_level = case_when(
    yearstea <= cuts[1] ~ "Low",
    yearstea > cuts[1] & yearstea <= cuts[2] ~ "Med",
    yearstea > cuts[2] ~ "Hih"
  )) %>%
  mutate(tea_level = factor(tea_level, levels = c("Low", "Med", "Hih")))


cuts2 <- quantile(data$yearstea, probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)

# Create the new factor
data <- data %>%
  mutate(tea_level2 = case_when(
    yearstea <= cuts2[1] ~ "L",
    yearstea > cuts2[1] & yearstea <= cuts2[2] ~ "LM",
    yearstea > cuts2[2] & yearstea <= cuts2[3] ~ "M",
    yearstea > cuts2[3] & yearstea <= cuts2[4] ~ "HM",
    yearstea > cuts2[4] ~ "H"
  )) %>%
  mutate(tea_level2 = factor(tea_level2, levels = c("L", "LM", "M", "HM", "H")))

# Check the split
table(data$tea_level)


# ---------------------------------------------------Analysis Tables--------------------------------------------------------------------------

# analyze all data
analysis_table_lvl_1 <- data %>%
  summarise(
    N_Obs = n(),
    mean = mean(mathgain, na.rm = TRUE),
    std_dev = sd(mathgain, na.rm = TRUE),
    minimum = min(mathgain, na.rm = TRUE),
    maximum = max(mathgain, na.rm = TRUE),
    mean_mathprep = mean(mathprep, na.rm = TRUE),
    sd_mathprep = sd(mathprep, na.rm = TRUE),
    mean_ses = mean(ses, na.rm = TRUE),
    sd_ses = sd(ses, na.rm = TRUE)
  )

print(kable(analysis_table_lvl_1, 
            col.names = c("N",  "mean_mathgain", "std_dev_mathgain", "min_mathgain", "max_mathgain", 
                          "mean_mathkind", "sd_mathkind", 
                          "mean_ses", "sd_ses"),
            digits = 2, 
            caption = "                                  Analysis Variables: LVL 1 factors\n-----------------------------------------------------------------------------------------------------------------"))

# analyze all data
analysis_table_lvl_2 <- data %>%
    group_by(classid) %>%
    summarize(
        yearstea = first(yearstea),
        mathprep = first(mathprep)
    ) %>%
    summarize(
        n_classes = n(),
        mean_yearstea = mean(yearstea, na.rm = TRUE),
        sd_yearstea = sd(yearstea, na.rm = TRUE),
        mean_mathprep = mean(mathprep, na.rm = TRUE),
        sd_mathprep = sd(mathprep, na.rm = TRUE)
    )

print(kable(analysis_table_lvl_2, 
            col.names = c("N (Classes)",
                          "mean_yearstea", "sd_yearstea", 
                          "mean_mathprep", "sd_mathprep"), 
            digits = 2, 
            caption = "         Analysis Variable : LVL 2 factors (Classroom Level)\n----------------------------------------------------------------"))


# Analyze by sex
analysis_table_sex <- data %>%
  group_by(sex) %>% 
  summarise(
    N_Obs = n(),
    mean = mean(mathgain, na.rm = TRUE),
    std_dev = sd(mathgain, na.rm = TRUE),
    minimum = min(mathgain, na.rm = TRUE),
    maximum = max(mathgain, na.rm = TRUE),
  ) %>%
  arrange(sex)

print(kable(analysis_table_sex, 
            col.names = c("Sex", "N",  "mean", "std_dev", "minimum", "maximum"),
            digits = 2, 
            caption = "     Analysis Variable : MathGain\n------------------------------------------------"))

# Analyze by minority
analysis_table_minor <- data %>%
  group_by(minority) %>% 
  summarise(
    N_Obs = n(),
    mean = mean(mathgain, na.rm = TRUE),
    std_dev = sd(mathgain, na.rm = TRUE),
    minimum = min(mathgain, na.rm = TRUE),
    maximum = max(mathgain, na.rm = TRUE),
  ) %>%
  arrange(minority)

print(kable(analysis_table_minor, 
            col.names = c("Minority", "N",  "mean", "std_dev", "minimum", "maximum"),
            digits = 2, 
            caption = "      Analysis Variable : MathGain\n--------------------------------------------------"))

# Cross Minority * Sex
analysis_table_crossed <- data %>%
  group_by(minority, sex) %>% 
  summarise(
    N_Obs = n(),
    mean = mean(mathgain, na.rm = TRUE),
    std_dev = sd(mathgain, na.rm = TRUE),
    minimum = min(mathgain, na.rm = TRUE),
    maximum = max(mathgain, na.rm = TRUE),
  ) %>%
  arrange(minority, sex)


print(kable(analysis_table_crossed, 
            col.names = c("Minority", "Sex", "N",  "mean", "std_dev", "minimum", "maximum"),
            digits = 2, 
            caption = "          Analysis Variable : MathGain\n----------------------------------------------------------"))


# Analyze by class
class_data <- data %>%
    group_by(classid) %>%
    summarize(
        n_students = n(),
        years_tea = first(yearstea),
        prep = first(mathprep)
    ) %>%
    ungroup()

halfway <- nrow(class_data) / 2
left_side <- class_data[1:halfway, ]
right_side <- class_data[(halfway + 1):nrow(class_data), ]

square_table <- bind_cols(left_side, ` ` = "|", right_side)

print(square_table)


# ---------------------------------------------------BW plots----------------------------------------------------------------------------


# Boxplot by sex
bwplot(mathgain ~ sex, 
       data = data,
       layout = c(1, 1), 
       main = "MathGain by Sex Boxplot",
       xlab = "Sex", 
       ylab = "MathGain",
       panel = function(x, y, ...) {
         panel.bwplot(x, y, ..., 
                      pch = "|",          
                      coef = 1.5,         
                      do.out = TRUE)
         m <- aggregate(y ~ x, FUN = mean)
         panel.points(m$x, m$y, pch = 18, col = "black", cex = 1.5)
       },
       par.settings = list(
         box.rectangle = list(col = "black", lwd = 1), 
         box.umbrella = list(col = "black", lty = 1),  
         plot.symbol = list(col = "black", pch = 1)    
       ))


# Boxplot by minority
bwplot(mathgain ~ minority, 
       data = data,
       layout = c(1, 1), 
       main = "MathGain by Minority Boxplots",
       xlab = "Minority", 
       ylab = "MathGain",
       panel = function(x, y, ...) {
         panel.bwplot(x, y, ..., 
                      pch = "|",          
                      coef = 1.5,         
                      do.out = TRUE)
         m <- aggregate(y ~ x, FUN = mean)
         panel.points(m$x, m$y, pch = 18, col = "black", cex = 1.5)
       },
       par.settings = list(
         box.rectangle = list(col = "black", lwd = 1), 
         box.umbrella = list(col = "black", lty = 1),  
         plot.symbol = list(col = "black", pch = 1)    
       ))

# Boxplot by minority * sex
bwplot(mathgain ~ minority | sex, 
       data = data,
       layout = c(2, 1), 
       main = "MathGain by Sex * Minority Boxplots",
       xlab = "Minority", 
       ylab = "MathGain",
       panel = function(x, y, ...) {
         panel.bwplot(x, y, ..., 
                      pch = "|",          
                      coef = 1.5,         
                      do.out = TRUE)
         m <- aggregate(y ~ x, FUN = mean)
         panel.points(m$x, m$y, pch = 18, col = "black", cex = 1.5)
       },
       par.settings = list(
         box.rectangle = list(col = "black", lwd = 1), 
         box.umbrella = list(col = "black", lty = 1), 
         plot.symbol = list(col = "black", pch = 1)   
       ))


# ------------------ By Class -----------------------


bwplot(mathgain ~ factor(classid), 
       data = data,
       box.width = 0.4,
       main = "MathGain scores by class ID",
       xlab = "Class ID (Sorted by ID)",
       ylab = "MathGain",
       scales = list(
         x = list(
           draw = TRUE,      
           cex = 0.7
         )
       ),
       panel = function(x, y, ...) {
         panel.bwplot(x, y, ..., pch = "|")
         
         m <- aggregate(y ~ x, FUN = mean)
         panel.points(m$x, m$y, pch = 5, col = "black", cex = 1.1, lwd = 1.2)
         
         # Reference lines
         panel.abline(h = 0, lty = 2, col = "gray50")},
       par.settings = list(
         # Outliers
         plot.symbol = list(pch = 1, col = "black", cex = 0.6),
         # Box borders
         box.rectangle = list(col = "black", lwd = 1),
         # Whiskers
         box.umbrella = list(col = "black", lty = 1, lwd = 1)
       ))

# ----------------------------------------------Scatter plots------------------------------------------------------------------------------------

# MathGain ~ Mathscore
ggplot(data, aes(x = mathkind, y = mathgain)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", col = "red") +   
  labs(title = "Relationship between 2022 Math score and Math Gain",
       x = "Math Score (2022)",
       y = "Gain in Math Score") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal()


# Colored by sex
ggplot(data, aes(x = mathkind, y = mathgain, color = sex)) +
  geom_point(alpha = 0.6) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Math Gain by 2022 Score and Sex",
       x = "Math Score (2022)",
       y = "Gain in Math Score",
       color = "Student Sex") +
  theme_minimal()

# Colored by minority
ggplot(data, aes(x = mathkind, y = mathgain, color = minority)) +
    geom_point(alpha = 0.6) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_smooth(method = "lm", se = FALSE) + 
    labs(title = "Math Gain by 2022 Score and Minority",
         x = "Math Score (2022)",
         y = "Gain in Math Score",
         color = "Student Minority") +
    theme_minimal()


# ------------------ SES -----------------------

# MathGain ~ SES
ggplot(data, aes(x = ses, y = mathgain)) +
  geom_point(alpha = 0.5, color = "steelblue") + 
  geom_smooth(method = "lm", col = "red") +       
  labs(title = "Relationship between SES and Math Gain",
       x = "SES",
       y = "Gain in Math Score") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal()


# Colored by sex
ggplot(data, aes(x = ses, y = mathgain, color = sex)) +
  geom_point(alpha = 0.6) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Math Gain by SES and Sex",
       x = "SES",
       y = "Gain in Math Score",
       color = "Student Sex") +
  theme_minimal()

# Colored by minority
ggplot(data, aes(x = ses, y = mathgain, color = minority)) +
  geom_point(alpha = 0.6) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Math Gain by SES and Minority",
       x = "SES",
       y = "Gain in Math Score",
       color = "Student Minority") +
  theme_minimal()


# MathGain ~ YearStea
ggplot(data, aes(x = yearstea, y = mathgain, color = classid)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "red") +       
    labs(title = "Relationship between YearsTea and Math Gain",
         x = "YearsTea",
         y = "Gain in Math Score") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    theme_minimal()


# Colored by sex
ggplot(data, aes(x = yearstea, y = mathgain, color = sex)) +
    geom_point(alpha = 0.6) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_smooth(method = "lm", se = FALSE) + 
    labs(title = "Math Gain by YearsTea and Sex",
         x = "YearsTea",
         y = "Gain in Math Score",
         color = "Student Sex") +
    theme_minimal()

# Colored by minority
ggplot(data, aes(x = yearstea, y = mathgain, color = minority)) +
    geom_point(alpha = 0.6) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_smooth(method = "lm", se = FALSE) + 
    labs(title = "Math Gain by YearsTea and Minority",
         x = "YearsTea",
         y = "Gain in Math Score",
         color = "Student Minority") +
    theme_minimal()

# Colored by tea_level level
ggplot(data, aes(x = yearstea, y = mathgain, color = tea_level)) +
    geom_point(alpha = 0.6) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_smooth(method = "lm", se = FALSE) + 
    labs(title = "Math Gain by YearsTea Level",
         x = "YearsTea",
         y = "Gain in Math Score",
         color = "YearsTea Level") +
    theme_minimal()

# ------------------ MathPrep -----------------------

# MathGain ~ MathPrep
ggplot(data, aes(x = mathprep, y = mathgain, color = classid)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", col = "red") +       
  labs(title = "Relationship between MathPrep and Math Gain",
       x = "MathPrep",
       y = "Gain in Math Score") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal()


# Colored by sex
ggplot(data, aes(x = mathprep, y = mathgain, color = sex)) +
  geom_point(alpha = 0.6) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Math Gain by MathPrep and Sex",
       x = "MathPrep",
       y = "Gain in Math Score",
       color = "Student Sex") +
  theme_minimal()

# Colored by minority
ggplot(data, aes(x = mathprep, y = mathgain, color = minority)) +
    geom_point(alpha = 0.6) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_smooth(method = "lm", se = FALSE) + 
    labs(title = "Math Gain by MathPrep and Minority",
         x = "MathPrep",
         y = "Gain in Math Score",
         color = "Student Minority") +
    theme_minimal()

# Colored by YearsTea level
ggplot(data, aes(x = mathprep, y = mathgain, color = tea_level)) +
    geom_point(alpha = 0.6) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_smooth(method = "lm", se = FALSE) + 
    labs(title = "Math Gain by MathPrep and YearsTea Level",
         x = "MathPrep",
         y = "Gain in Math Score",
         color = "YearsTea level") +
    theme_minimal()

# Model 1 Model with Loaded Mean Structure
model.1.fit <- lme(mathgain ~ sex + minority + (sex:minority) + mathkind + yearstea + (sex:yearstea) + (mathkind:minority),
                     random = ~ 1|classid,
                     data = data, method = "REML")
summary(model.1.fit)
anova(model.1.fit)

# Model 1A Excludes the random intercepts effects
model.1a.fit <- gls(mathgain ~ sex + minority + (sex:minority) + mathkind + yearstea + (sex:yearstea) + (mathkind:minority), 
                    data = data)
anova(model.1.fit, model.1a.fit) # Test Hypothesis 1
# Keeping M1

# Model 2A Heterogenous Residual by tea_level
model.2a.fit <- lme(mathgain ~ sex + minority + (sex:minority) + mathkind + yearstea + (sex:yearstea) + (mathkind:minority),
                   random = ~ 1|classid, 
                   data = data, method = "REML", 
                   weights = varIdent(form = ~1 | tea_level))
summary(model.2a.fit)
anova(model.1.fit, model.2a.fit) # Hypothesis Test 2
# Keeping M2A

# Model 2B Heterogenous Residual by tea_level2 -- 
model.2b.fit <- lme(mathgain ~ sex + minority + (sex:minority) + mathkind + yearstea + (sex:yearstea) + (mathkind:minority),
                   random = ~ 1|classid, 
                   data = data, method = "REML", 
                   weights = varIdent(form = ~1 | tea_level2))
summary(model.2b.fit)
anova(model.1.fit, model.2b.fit) # Test Hypothesis 3
# Keeping M2B
anova(model.2a.fit, model.2b.fit) # Test Hypothesis 4
# Keeping M2A

# Model 3 Remove sex 
anova(model.2a.fit) # Hypothesis Test 5
# Keeping M3
model.3.fit <- lme(mathgain ~ minority + (sex:minority) + mathkind + yearstea + (sex:yearstea) + (mathkind:minority),
                    random = ~ 1|classid, 
                    data = data, method = "REML", 
                    weights = varIdent(form = ~1 | tea_level))
summary(model.3.fit)
anova(model.3.fit) # Hypothesis Test 6
# Keeping M3A

# Model 3A Remove minority
model.3a.fit <- lme(mathgain ~ mathkind + yearstea + (sex:yearstea) + (sex:minority) + (mathkind:minority),
                   random = ~ 1|classid, 
                   data = data, method = "REML", 
                   weights = varIdent(form = ~1 | tea_level))
anova(model.3a.fit) # Hypothesis Test 7
# Keeping M3B

# Model 3B Remove mathkind:minority
model.3b.fit <- lme(mathgain ~ mathkind + yearstea + (sex:yearstea) + (sex:minority),
                    random = ~ 1|classid, 
                    data = data, method = "REML", 
                    weights = varIdent(form = ~1 | tea_level))
anova(model.3b.fit) # Hypothesis Test 8
# Keeping M3B

model.2a.ml.fit <- lme(mathgain ~ sex + minority + (sex:minority) + mathkind + yearstea + (sex:yearstea) + (mathkind:minority),
                       random = ~ 1|classid, 
                       data = data, method = "ML", 
                       weights = varIdent(form = ~1 | tea_level))

model.3b.ml.fit <- lme(mathgain ~ mathkind + yearstea + (sex:yearstea) + (sex:minority),
                    random = ~ 1|classid, 
                    data = data, method = "ML", 
                    weights = varIdent(form = ~1 | tea_level))
anova(model.2a.ml.fit, model.3b.ml.fit) # Test Hypothesis 9
# Keeping M3B due to simplicity and BIC

# Model 4 Adding Heterogeneous Residuals by tea_level and sex
model.4.fit <- lme(mathgain ~ mathkind + yearstea + (sex:yearstea) + (sex:minority),
                       random = ~ 1|classid, 
                       data = data, method = "REML", 
                       weights = varComb(varIdent(form = ~1 | sex), 
                                         varIdent(form = ~1 | tea_level)))
anova(model.3b.fit, model.4.fit) # Test Hypothesis 10 
# Keeping M4


model.4.ml.fit <- lme(mathgain ~ mathkind + yearstea + (sex:yearstea) + (sex:minority),
                   random = ~ 1|classid, 
                   data = data, method = "ML", 
                   weights = varComb(varIdent(form = ~1 | sex), 
                                     varIdent(form = ~1 | tea_level)))

model.5.ml.fit <- lme(mathgain ~ mathkind + mathprep + yearstea + (sex:yearstea) + (sex:minority),
                   random = ~ mathkind|classid, 
                   data = data, method = "ML", 
                   weights = varComb(varIdent(form = ~1 | sex), 
                                     varIdent(form = ~1 | tea_level)))
summary(model.5.ml.fit)
anova(model.4.ml.fit, model.5.ml.fit) # Test Hypothesis 11







# Later diagnostic plot material

# This plots the "Pure" effect of SES from your LME model
plot(effect("ses", model.5.ml.fit), 
     main="Effect of SES on Math Gain (Controlled)",
     ylab="Predicted Math Gain",
     xlab="Socioeconomic Status")
