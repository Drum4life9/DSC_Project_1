source("b_exploration.R")
source("c_models.R")

# ------------------------------------------------------------------------------------------

# Extract the random effects (BLUPs)
random_effects <- ranef(final.model.fit)

# Rename for clarity
colnames(random_effects) <- c("u0j_Intercept", "u1j_Slope")

# Add the Classroom IDs as a column
random_effects$classid <- rownames(random_effects)

# Sort by the Intercept offset (u0i) to see high-performing classrooms
top_classrooms <- random_effects[order(-random_effects$u0j_Intercept), ]

# View the top 5
head(top_classrooms, 5)


# ------------------------------------------------------------------------------------------

re_df <- ranef(final.model.fit, augFrame = TRUE)

re_df$classid <- rownames(re_df)
re_df$classid <- factor(re_df$classid, levels = sort(as.numeric(unique(re_df$classid))))

# Plot Intercepts ordered by classid
dotplot(classid ~ `(Intercept)`, data = re_df,
        xlab = "Random Intercept Offset (u0j)",
        main = "Classroom Intercepts (Ordered by ID)",
        panel = function(...) {
          panel.abline(v = 0, lty = "dashed", col = "red")
          panel.dotplot(...)
        })

# Plot mathkind Slopes ordered by classid
dotplot(classid ~ mathkind, data = re_df,
        xlab = "Random Slope Offset (u1j)",
        main = "MathKind Slopes (Ordered by ID)",
        panel = function(...) {
          panel.abline(v = 0, lty = "dashed", col = "red")
          panel.dotplot(...)
        })


# ------------------------------------------------------------------------------------------
# Create a subset excluding Class 37
data_no_37 <- subset(data, classid != "37")

# Re-run your final nlme model
model_no_37 <- lme(mathgain ~ sex + mathkind + (sex:minority) + ses,
                   random = ~ mathkind | classid, 
                   data = data_no_37, 
                   method = "REML", 
                   weights = varComb(varIdent(form = ~1 | sex), 
                                     varIdent(form = ~1 | tea_level)))

# Compare coefficients
summary(model_no_37)


# ------------------------------------------------------------------------------------------

# Extract random effects from the model without Class 37
re_no_37 <- ranef(model_no_37)

# Add ClassID and rename columns for clarity
re_no_37$classid <- rownames(re_no_37)
colnames(re_no_37) <- c("u0j_Intercept", "u1j_Slope", "classid")

# Sort by Intercept (descending) to find the new leaders
top_5_new <- re_no_37[order(-re_no_37$u0j_Intercept), ]

# View the top 5
head(top_5_new, 5)


# ------------------------------------------------------------------------------------------

re_df <- ranef(model_no_37, augFrame = TRUE)

re_df$classid <- rownames(re_df)
re_df$classid <- factor(re_df$classid, levels = sort(as.numeric(unique(re_df$classid))))

# Plot Intercepts ordered by classid
dotplot(classid ~ `(Intercept)`, data = re_df,
        xlab = "Random Intercept Offset (u0j)",
        main = "Classroom Intercepts (Ordered by ID)",
        panel = function(...) {
                panel.abline(v = 0, lty = "dashed", col = "red")
                panel.dotplot(...)
        })

# Plot mathkind Slopes ordered by classid
dotplot(classid ~ mathkind, data = re_df,
        xlab = "Random Slope Offset (u1j)",
        main = "MathKind Slopes (Ordered by ID)",
        panel = function(...) {
                panel.abline(v = 0, lty = "dashed", col = "red")
                panel.dotplot(...)
        })


# ------------------------------------------------------------------------------------------

data_no_37$resids <- resid(model_no_37, type = "response")

histogram(~ resids | sex, 
          data = data_no_37,
          layout = c(2, 1),
          aspect = 1,          
          type = "percent",    
          main = "Figure 3.6: Histograms of Conditional Raw Residuals",
          xlab = "Conditional Raw Residuals",
          ylab = "percent",
          col = "gray80",)

histogram(~ resids | tea_level, 
          data = data_no_37,
          layout = c(3, 1),
          aspect = 1,          
          type = "percent",    
          main = "Figure 3.6: Histograms of Conditional Raw Residuals",
          xlab = "Conditional Raw Residuals",
          ylab = "percent",
          col = "gray80",)

histogram(~ resids | sex:tea_level, 
          data = data_no_37,
          layout = c(3, 2),
          aspect = 1,          
          type = "percent",    
          main = "Figure 3.6: Histograms of Conditional Raw Residuals",
          xlab = "Conditional Raw Residuals",
          ylab = "percent",
          col = "gray80",)


# ------------------------------------------------------------------------------------------

# fig 3.7 - Q-Q plot
mu_val <- round(mean(data_no_37$resids), 4)
sigma_val <- round(sd(data_no_37$resids), 4)
qqmath(~ resids | tea_level, 
       data = data_no_37,
       layout = c(1, 3),           
       main = "Figure 3.7: Normal Q-Q Plots of Conditional Raw Residuals",
       xlab = "Standard Normal Quantiles",
       ylab = "Conditional Raw Residuals",
       key = list(
           space = "bottom",
           columns = 2,
           text = list(c(as.expression(substitute(mu == m, list(m = mu_val))),
                         as.expression(substitute(sigma == s, list(s = sigma_val))))),
           points = list(pch = c(NA, NA)) 
       ),
       panel = function(x, ...) {
           panel.qqmath(x, ...)
           panel.qqmathline(x, ...)    # Adds the reference line (y = x logic)
       },
       par.settings = list(plot.symbol = list(pch = 1, col = "black", cex = 0.8)))

qqmath(~ resids | sex, 
       data = data_no_37,
       layout = c(1, 2),             
       main = "Figure 3.7: Normal Q-Q Plots of Conditional Raw Residuals",
       xlab = "Standard Normal Quantiles",
       ylab = "Conditional Raw Residuals",
       key = list(
               space = "bottom",
               columns = 2,
               text = list(c(as.expression(substitute(mu == m, list(m = mu_val))),
                             as.expression(substitute(sigma == s, list(s = sigma_val))))),
               points = list(pch = c(NA, NA)) 
       ),
       panel = function(x, ...) {
               panel.qqmath(x, ...)
               panel.qqmathline(x, ...)    # Adds the reference line (y = x logic)
       },
       par.settings = list(plot.symbol = list(pch = 1, col = "black", cex = 0.8)))

qqmath(~ resids | sex:tea_level, 
       data = data_no_37,
       layout = c(3, 2),           
       main = "Figure 3.7: Normal Q-Q Plots of Conditional Raw Residuals",
       xlab = "Standard Normal Quantiles",
       ylab = "Conditional Raw Residuals",
       key = list(
               space = "bottom",
               columns = 2,
               text = list(c(as.expression(substitute(mu == m, list(m = mu_val))),
                             as.expression(substitute(sigma == s, list(s = sigma_val))))),
               points = list(pch = c(NA, NA)) # Keeps it as text only
       ),
       panel = function(x, ...) {
               panel.qqmath(x, ...)
               panel.qqmathline(x, ...)    # Adds the reference line (y = x logic)
       },
       par.settings = list(plot.symbol = list(pch = 1, col = "black", cex = 0.8)))


# ------------------------------------------------------------------------------------------

# Ensure residuals are attached to the data_no_37 frame
data_no_37$resids <- resid(model_no_37)

# Filter for the specific group where we see the outlier
group_check <- subset(data_no_37, tea_level == "Med")

# Sort by largest absolute residual to find the "top" outlier
outlier_candidate <- group_check[order(-abs(group_check$resids)), ]

# View the top 5 students in this group
head(outlier_candidate, 5)

# Extract Student 41
student_41 <- data_no_37[data_no_37$childid == 41, ]

# Extract the "Typical" student in the same group (Male, Med Teacher)
typical_group <- subset(data_no_37, sex == "male" & tea_level == "Med")
group_means <- colMeans(typical_group[, c("mathkind", "ses", "mathgain")], na.rm = TRUE)

# Combine for a comparison table
comparison <- rbind(student_41[, c("mathkind", "ses", "mathgain")], group_means)

rownames(comparison) <- c("Student 41", "Group Average")
print(comparison)

# ------------------------------------------------------------------------------------------

# fig. 3.8
data_no_37$fitted <- fitted(model_no_37)
data_no_37$resids <- resid(model_no_37)

xyplot(resids ~ fitted | tea_level, 
       data = data_no_37,
       layout = c(3, 1),
       main = "Figure 3.8: Conditional Residuals vs. Predicted Values",
       xlab = "Linear predictor",
       ylab = "Residual",
       # Add a reference line at zero
       panel = function(x, y, ...) {
           panel.xyplot(x, y, ...)
           panel.abline(h = 0, lty = 2, col = "gray50") # Dashed zero line
       },
       par.settings = list(plot.symbol = list(pch = 1, col = "black", cex = 0.8)))

xyplot(resids ~ fitted | sex, 
       data = data_no_37,
       layout = c(2, 1),
       main = "Figure 3.8: Conditional Residuals vs. Predicted Values",
       xlab = "Linear predictor",
       ylab = "Residual",
       # Add a reference line at zero
       panel = function(x, y, ...) {
               panel.xyplot(x, y, ...)
               panel.abline(h = 0, lty = 2, col = "gray50") # Dashed zero line
       },
       par.settings = list(plot.symbol = list(pch = 1, col = "black", cex = 0.8)))

xyplot(resids ~ fitted | sex:tea_level, 
       data = data_no_37,
       layout = c(3, 2),
       main = "Figure 3.8: Conditional Residuals vs. Predicted Values",
       xlab = "Linear predictor",
       ylab = "Residual",
       # Add a reference line at zero
       panel = function(x, y, ...) {
               panel.xyplot(x, y, ...)
               panel.abline(h = 0, lty = 2, col = "gray50") # Dashed zero line
       },
       par.settings = list(plot.symbol = list(pch = 1, col = "black", cex = 0.8)))


# ------------------------------------------------------------------------------------------

# fig. 3.9
data_no_37$stud_resids <- resid(model_no_37, type = "normalized")
data_no_37$classid_ordered <- reorder(data_no_37$classid, data_no_37$stud_resids, FUN = median)

bwplot(stud_resids ~ classid_ordered, 
       data = data_no_37,
       box.width = 0.5,
       main = "Figure 3.9: Studentized Residuals by Classroom ID",
       xlab = "Class ID (Ordered by Median Residual)",
       ylab = "Studentized Residuals",
       scales = list(
               x = list(
                       draw = TRUE, 
                       rot = 90,
                       cex = 0.6
               )
       ),
       panel = function(x, y, ...) {
               panel.bwplot(x, y, ..., pch = "|")
               
               # Reference lines at 0 and +/- 2 (standard thresholds for outliers)
               panel.abline(h = 0, lty = 1, col = "black")
               panel.abline(h = c(-2, 2), lty = 2, col = "gray40")
               
               # Add means as open diamonds
               means <- aggregate(y ~ x, FUN = mean)
               panel.points(means$x, means$y, pch = 5, col = "black", cex = 0.8)
       },
       par.settings = list(
               plot.symbol = list(pch = 1, col = "black", cex = 0.5),
               box.rectangle = list(col = "black", fill = "transparent"),
               box.umbrella = list(col = "black", lty = 1)
       ))
