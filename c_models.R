source("b_exploration.R")

# ------------------------------------------------------------------------------------------

# Model 1 Model with Loaded Mean Structure
model.1.fit <- lme(mathgain ~ sex + minority + (sex:minority) + mathkind + 
                     yearstea + (sex:yearstea) + (mathkind:minority) + ses,
                     random = ~ 1|classid,
                     data = data, method = "REML")
anova(model.1.fit)


# ------------------------------------------------------------------------------------------

# Model 1A Excludes the random intercepts effects
model.1a.fit <- gls(mathgain ~ sex + minority + (sex:minority) + mathkind + 
                      yearstea + (sex:yearstea) + (mathkind:minority) + ses, 
                    data = data)
# Hypothesis 1 --> Null Hypothesis: Drop uj (variance of random effect = 0)
# --> Alternative Hypothesis: Keep uj (variance of random effect > 0)
anova(model.1.fit, model.1a.fit) # Test Hypothesis 1
# Keeping M1  --> reject Null Hypothesis


# ------------------------------------------------------------------------------------------

# Model 2A Heterogeneous Residual by tea_level
model.2a.fit <- lme(mathgain ~ sex + minority + (sex:minority) + mathkind + 
                        yearstea + (sex:yearstea) + (mathkind:minority) + ses,
                    random = ~ 1|classid, 
                    data = data, method = "REML", 
                    weights = varIdent(form = ~1 | tea_level))

# Hypothesis 2 --> Null Hypothesis: Homogeneous residual variance (all variances of tea_level groups are the same)
# Alternative Hypothesis: Residual variances are not all equal
anova(model.1.fit, model.2a.fit) # Test Hypothesis 2
# Keeping M2A --> reject Null Hypothesis


# ------------------------------------------------------------------------------------------

# Model 2B Heterogeneous Residual by tea_level2
model.2b.fit <- lme(mathgain ~ sex + minority + (sex:minority) + mathkind + 
                      yearstea + (sex:yearstea) + (mathkind:minority) + ses,
                   random = ~ 1|classid, 
                   data = data, method = "REML", 
                   weights = varIdent(form = ~1 | tea_level2))
# Hypothesis 3 --> Null Hypothesis: Homogeneous residual variance (all variances of tea_level2 groups are the same)
# Alternative Hypothesis: Residual variances are not all equal
anova(model.1.fit, model.2b.fit) # Test Hypothesis 3
# Keeping M2B --> reject Null Hypothesis

# ------------------------------------------------------------------------------------------

# AIC 4 --> Comparing AIC and BIC of M2A and M2B
anova(model.2a.fit, model.2b.fit) # Test AIC 4
# Keeping M2A --> AIC and BIC for M2A are lower 
model.2a.ml.fit <- lme(mathgain ~ sex + minority + (sex:minority) + mathkind + 
                         yearstea + (sex:yearstea) + (mathkind:minority) + ses,
                      random = ~ 1|classid, 
                      data = data, method = "ML", 
                      weights = varIdent(form = ~1 | tea_level))


# ------------------------------------------------------------------------------------------

# Hypothesis 5 --> Null Hypothesis: Drop sex (B_sex = 0)
# Alternative Hypothesis: Keep sex (B_sex != 0)
drop1(model.2a.ml.fit, test = "Chisq") # Test Hypothesis 5
# Keeping M3 --> fail to reject Null Hypothesis because p_value > 0.05

# Model 3 Removed sex 
model.3.ml.fit <- lme(mathgain ~ minority + (sex:minority) + mathkind + yearstea + 
                     (sex:yearstea) + (mathkind:minority) + ses,
                    random = ~ 1|classid, 
                    data = data, method = "ML", 
                    weights = varIdent(form = ~1 | tea_level))


# ------------------------------------------------------------------------------------------

# Hypothesis 6 --> Null Hypothesis: Drop minority (B_minority = 0)
# Alternative Hypothesis: Keep minority (B_minority != 0)
drop1(model.3.ml.fit, test = "Chisq") # Test Hypothesis 6
# Keeping M3A --> fail to reject Null Hypothesis because p_value > 0.05

# Model 3A Removed minority and add sex
model.3a.ml.fit <- lme(mathgain ~ sex + mathkind + yearstea + (sex:yearstea) + 
                      (sex:minority) + (mathkind:minority) + ses,
                   random = ~ 1|classid, 
                   data = data, method = "ML", 
                   weights = varIdent(form = ~1 | tea_level))


# ------------------------------------------------------------------------------------------

# Hypothesis 7 --> Null Hypothesis: Drop mathkind:minority (B_mathkind:minority = 0)
# Alternative Hypothesis: Keep mathkind:minority (B_mathkind:minority != 0)
drop1(model.3a.ml.fit, test = "Chisq") # Test Hypothesis 7
# Keeping M3B --> fail to reject Null Hypothesis because p_value > 0.05

# Model 3B Removed mathkind:minority
model.3b.ml.fit <- lme(mathgain ~ sex + mathkind + yearstea + (sex:yearstea) + 
                      (sex:minority) + ses,
                    random = ~ 1|classid, 
                    data = data, method = "ML", 
                    weights = varIdent(form = ~1 | tea_level))


# ------------------------------------------------------------------------------------------

# Hypothesis 8 --> Null Hypothesis: Drop yearstea:sex (B_yearstea:sex = 0)
# Alternative Hypothesis: Keep yearstea:sex (B_yearstea:sex != 0)
drop1(model.3b.ml.fit, test = "Chisq") # Test Hypothesis 8
# Keeping M3B --> fail to reject Null Hypothesis because p_value > 0.05

# Model 3C Removed yearstea:sex
model.3c.ml.fit <- lme(mathgain ~ sex + mathkind + yearstea + (sex:minority) + ses,
                    random = ~ 1|classid, 
                    data = data, method = "ML", 
                    weights = varIdent(form = ~1 | tea_level))


# ------------------------------------------------------------------------------------------

# Hypothesis 9 --> Null Hypothesis: Drop yearstea:sex (B_yearstea = 0)
# Alternative Hypothesis: Keep yearstea:sex (B_yearstea != 0)
drop1(model.3c.ml.fit, test = "Chisq") # Test Hypothesis 9
# Keeping M3C --> fail to reject Null Hypothesis because p_value > 0.05

# Model 3D Removed yearstea
model.3d.ml.fit <- lme(mathgain ~ sex + mathkind + (sex:minority) + ses,
                    random = ~ 1|classid, 
                    data = data, method = "ML", 
                    weights = varIdent(form = ~1 | tea_level))


# ------------------------------------------------------------------------------------------

# Hypothesis 10 --> Null Hypothesis: Drop any other fixed effects (B_x = 0; x = any fixed effect)
# Alternative Hypothesis: Keep all fixed effects (B_x != 0; x = any fixed effect)
drop1(model.3d.ml.fit, test = "Chisq") # Test Hypothesis 10
# Keeping M3D --> reject Null Hypothesis

model.2a.ml.fit <- lme(mathgain ~ sex + minority + (sex:minority) + mathkind + 
                         yearstea + (sex:yearstea) + (mathkind:minority) + ses,
                       random = ~ 1|classid, 
                       data = data, method = "ML", 
                       weights = varIdent(form = ~1 | tea_level))


# ------------------------------------------------------------------------------------------

# Hypothesis 11 --> Null Hypothesis: Drop minority, sex:yearstea, yearstea, and mathkind:minority
# Alternative Hypothesis: Keep minority, sex:yearstea, and mathkind:minority
anova(model.2a.ml.fit, model.3d.ml.fit) # Test Hypothesis 11
# Keeping M3D due to AIC and BIC --> fail to reject Null Hypothesis

model.3d.fit <- lme(mathgain ~ sex + mathkind + (sex:minority) + ses,
                       random = ~ 1|classid, 
                       data = data, method = "REML", 
                       weights = varIdent(form = ~1 | tea_level))

# Model 4 Adding Heterogeneous Residuals by tea_level and sex
model.4.fit <- lme(mathgain ~ sex + mathkind + (sex:minority) + ses,
                       random = ~ 1|classid, 
                       data = data, method = "REML", 
                       weights = varComb(varIdent(form = ~1 | sex), 
                                         varIdent(form = ~1 | tea_level)))


# ------------------------------------------------------------------------------------------

# Hypothesis 12 --> Null Hypothesis: Homogeneous residual variance (all variances of tea_level and sex groups are the same)
# Alternative Hypothesis: Heterogeneous residual variance (all variances of tea_level and sex groups are the different)
anova(model.3d.fit, model.4.fit) # Test Hypothesis 12
# Keeping M4 --> Reject Null Hypothesis
anova(model.4.fit)

# Add random slope between mathkind and classid
model.5.fit <- lme(mathgain ~ sex + mathkind + (sex:minority) + ses,
                   random = ~ mathkind|classid, 
                   data = data, method = "REML", 
                   weights = varComb(varIdent(form = ~1 | sex), 
                                     varIdent(form = ~1 | tea_level)))
anova(model.5.fit)


# ------------------------------------------------------------------------------------------

# Hypothesis 13 --> Null Hypothesis: Drop u1j (variance of u1j = 0)
# Alternative Hypothesis: Keep u1j (variance of u1j != 0)
anova(model.4.fit, model.5.fit) # Hypothesis Test 13
# Keep M5 --> Reject Null Hypothesis


# ------------------------------------------------------------------------------------------

final.model.fit <- lme(mathgain ~ sex + mathkind + (sex:minority) + ses,
                       random = ~ mathkind|classid, 
                       data = data, method = "REML", 
                       weights = varComb(varIdent(form = ~1 | sex), 
                                         varIdent(form = ~1 | tea_level)))
summary(final.model.fit)
