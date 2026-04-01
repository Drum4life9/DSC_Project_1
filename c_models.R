source("b_exploration.R")

# ------------------------------------------------------------------------------------------

# Model 1 Model with Loaded Mean Structure
model.1.fit <- lme(mathgain ~ sex + minority + (sex:minority) + mathkind + 
                     yearstea + (sex:yearstea) + (mathkind:minority) + ses,
                   random = ~ 1|classid,
                   data = data, method = "REML")
# ------------------------------------------------------------------------------
# Model 2A Excludes the random intercepts effects
model.2A.fit <- gls(mathgain ~ sex + minority + (sex:minority) + mathkind + 
                      yearstea + (sex:yearstea) + (mathkind:minority) + ses, 
                    data = data)
# Hypothesis 1 --> Null Hypothesis: Drop u0j (variance of random intercept effect = 0)
# --> Alternative Hypothesis: Keep u0j (variance of random intercept effect > 0)
# Test Hypothesis 1
anova(model.1.fit, model.2A.fit) 
# Reject Null Hypothesis --> Keep Model 1

# Model 2B Add random slope between mathkind and classid
model.2B.fit <- lme(mathgain ~ sex + minority + (sex:minority) + mathkind + 
                      yearstea + (sex:yearstea) + (mathkind:minority) + ses,
                   random = ~ mathkind|classid, 
                   data = data, method = "REML")
# Hypothesis 2 --> Null Hypothesis: Drop u1j (variance of random slope effect = 0)
# Alternative Hypothesis: Keep u1j (variance of random slope effect != 0)
# Test Hypothesis 2
anova(model.1.fit, model.2B.fit) 
# Reject Null Hypothesis --> Keep Model 2B

# Model 3A Heterogeneous Residual by tea_level
model.3A.fit <- lme(mathgain ~ sex + minority + (sex:minority) + mathkind + 
                      yearstea + (sex:yearstea) + (mathkind:minority) + ses,
                    random = ~ mathkind|classid, 
                    data = data, method = "REML", 
                    weights = varIdent(form = ~1 | tea_level))
# Hypothesis 3 --> Null Hypothesis: Homogeneous residual variance (all variances of tea_level groups are the same)
# Alternative Hypothesis: Residual variances are not all equal
# Test Hypothesis 3
anova(model.2B.fit, model.3A.fit) 
# Reject Null Hypothesis --> Keep Model 3A

# Model 3B Heterogeneous Residual by sex
model.3B.fit <- lme(mathgain ~ sex + minority + (sex:minority) + mathkind + 
                      yearstea + (sex:yearstea) + (mathkind:minority) + ses,
                    random = ~ mathkind|classid, 
                    data = data, method = "REML", 
                    weights = varIdent(form = ~1 | sex))
# Hypothesis 4 --> Null Hypothesis: Homogeneous residual variance (all variances of sex groups are the same)
# Alternative Hypothesis: Residual variances are not all equal
# Test Hypothesis 4
anova(model.2B.fit, model.3B.fit)
# Reject Null Hypothesis --> Keep Model 3B

# Test 5 --> Comparing AIC and BIC of Model 3A and Model 3B
anova(model.3A.fit, model.3B.fit)

# Model 3C Heterogeneous Residual by tea_level and sex
model.3C.fit <- lme(mathgain ~ sex + minority + (sex:minority) + mathkind + 
                      yearstea + (sex:yearstea) + (mathkind:minority) + ses,
                    random = ~ mathkind|classid, 
                    data = data, method = "REML", 
                    weights = varComb(varIdent(form = ~1 | sex), 
                                      varIdent(form = ~1 | tea_level)))
# Hypothesis 6 --> Null Hypothesis: Homogeneous residual variance (all variances of tea_level and sex groups are the same)
# Alternative Hypothesis: Residual variances are not all equal
anova(model.2B.fit, model.3C.fit)
# Reject Null Hypothesis --> Keep Model 3C

# Test 7 --> Comparing AIC and BIC of Model 3A and Model 3B
anova(model.3A.fit, model.3B.fit, model.3C.fit)
# Keep Model 3C 

# Hypothesis 8 --> Null Hypothesis: Drop minority (B_minority = 0)
# Alternative Hypothesis: Keep Minority (B_minority != 0)
# Test Hypothesis 8
anova(model.3C.fit)
# Fail to reject Null Hypothesis --> Drop B_minority 

# Model 4A Removed minority 
model.4A.ml.fit <- lme(mathgain ~ sex + (sex:minority) + mathkind + yearstea + 
                         (sex:yearstea) + (mathkind:minority) + ses,
                       random = ~ mathkind|classid, 
                       data = data, method = "ML", 
                       weights = varComb(varIdent(form = ~1 | sex), 
                                         varIdent(form = ~1 | tea_level)))
# Model 4B Removed mathkind:minority
model.4B.ml.fit <- lme(mathgain ~ sex + (sex:minority) + mathkind + yearstea + 
                         (sex:yearstea) + ses,
                       random = ~ mathkind|classid, 
                       data = data, method = "ML", 
                       weights = varComb(varIdent(form = ~1 | sex), 
                                         varIdent(form = ~1 | tea_level)))
# Hypothesis 9 --> Null Hypothesis: Drop mathkind:minority (B_mathkind:minority = 0)
# Alternative Hypothesis: Keep mathkind:minority (B_mathkind:minority != 0)
# Test Hypothesis 9
anova(model.4A.ml.fit, model.4B.ml.fit)
# Fail to reject Null Hypothesis --> Keep Model 4B

# Model 4C Removed yearstea
model.4C.ml.fit <- lme(mathgain ~ sex + (sex:minority) + mathkind + 
                         (sex:yearstea) + ses,
                       random = ~ mathkind|classid, 
                       data = data, method = "ML", 
                       weights = varComb(varIdent(form = ~1 | sex), 
                                         varIdent(form = ~1 | tea_level)))
# Hypothesis 10 --> Null Hypothesis: Drop yearstea (B_yearstea = 0)
# Alternative Hypothesis: Keep yearstea (B_yearstea != 0)
# Test Hypothesis 10
anova(model.4B.ml.fit, model.4C.ml.fit)
drop1(model.4C.ml.fit, test = "Chisq")
# Fail to reject Null Hypothesis --> Keep Model 4C

# Model 4D Removed sex:yearstea
model.4D.ml.fit <- lme(mathgain ~ sex + (sex:minority) + mathkind + ses,
                       random = ~ mathkind|classid, 
                       data = data, method = "ML", 
                       weights = varComb(varIdent(form = ~1 | sex), 
                                         varIdent(form = ~1 | tea_level)))
# Hypothesis 11 --> Null Hypothesis: Drop sex:yearstea (B_sex:yearstea = 0)
# Alternative Hypothesis: Keep sex:yearstea (B_sex:yearstea != 0)
# Test Hypothesis 11
anova(model.4C.ml.fit, model.4D.ml.fit)
# Fail to reject Null Hypothesis --> Keep Model 4D

# Hypothesis 12 --> Null Hypothesis: Drop minority, sex:yearstea, yearstea, and mathkind:minority
# Alternative Hypothesis: Keep minority, sex:yearstea, and mathkind:minority
model.3C.ml.fit <- lme(mathgain ~ sex + minority + (sex:minority) + mathkind + 
                      yearstea + (sex:yearstea) + (mathkind:minority) + ses,
                    random = ~ mathkind|classid, 
                    data = data, method = "ML", 
                    weights = varComb(varIdent(form = ~1 | sex), 
                                      varIdent(form = ~1 | tea_level)))
# Test Hypothesis 12
anova(model.3C.ml.fit, model.4D.ml.fit)
# Fail to reject Null Hypothesis --> Keep Model 4D

model.4D.fit <- lme(mathgain ~ sex + (sex:minority) + mathkind + ses,
                       random = ~ mathkind|classid, 
                       data = data, method = "REML", 
                       weights = varComb(varIdent(form = ~1 | sex), 
                                         varIdent(form = ~1 | tea_level)))

# ------------------------------------------------------------------------------
final.model.fit <- lme(mathgain ~ sex + mathkind + (sex:minority) + ses,
                       random = ~ mathkind|classid, 
                       data = data, method = "REML", 
                       weights = varComb(varIdent(form = ~1 | sex), 
                                         varIdent(form = ~1 | tea_level)))
summary(final.model.fit)
