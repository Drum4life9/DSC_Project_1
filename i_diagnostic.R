source("b_exploration.R")
source("models.R")

# Later diagnostic plot material

# This plots the "Pure" effect of SES from your LME model
plot(effect("ses", model.5.ml.fit), 
     main="Effect of SES on Math Gain (Controlled)",
     ylab="Predicted Math Gain",
     xlab="Socioeconomic Status")



ratpup$resids <- resid(model3.3.reml.fit, type = "response")
ratpup$pooled_treat <- ifelse(ratpup$treatment == "Control", 
                              "Control", "High/Low")
ratpup$pooled_treat <- factor(ratpup$pooled_treat, levels = c("Control", "High/Low"))

histogram(~ resids | pooled_treat, 
          data = ratpup,
          layout = c(2, 1),
          nint = 9,           # Number of bins
          aspect = 1,          # Square panels
          type = "percent",    # Matches book's vertical scale
          main = "Figure 3.6: Histograms of Conditional Raw Residuals",
          xlab = "Conditional Raw Residuals",
          ylab = "percent",
          col = "gray80",)


# fig 3.7 - Q-Q plot

mu_val <- round(mean(ratpup$resids), 4)
sigma_val <- round(sd(ratpup$resids), 4)
qqmath(~ resids | pooled_treat, 
       data = ratpup,
       layout = c(1, 2),             # 2 columns (Control, High/Low)
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
       # par.settings to match the textbook's clean look
       par.settings = list(plot.symbol = list(pch = 1, col = "black", cex = 0.8)))

# fig. 3.8
ratpup$fitted <- fitted(model3.3.reml.fit)
ratpup$resids <- resid(model3.3.reml.fit)

ratpup$pooled_treat <- factor(ratpup$pooled_treat, levels = c("High/Low", "Control"))


xyplot(resids ~ fitted | pooled_treat, 
       data = ratpup,
       layout = c(2, 1),
       main = "Figure 3.8: Conditional Residuals vs. Predicted Values",
       xlab = "Linear predictor",
       ylab = "Residual",
       # Add a reference line at zero
       panel = function(x, y, ...) {
           panel.xyplot(x, y, ...)
           panel.abline(h = 0, lty = 2, col = "gray50") # Dashed zero line
       },
       # par.settings for a clean, professional look
       par.settings = list(plot.symbol = list(pch = 1, col = "black", cex = 0.8)))


# fig. 3.9
ratpup$stud_resids <- resid(model3.3.reml.fit, type = "pearson")
ratpup$litter <- reorder(ratpup$litter, as.numeric(as.character(ratpup$litter)))

bwplot(stud_resids ~ litter, 
       data = ratpup,
       box.width = 0.4,
       main = "Figure 3.9: Studentized Residuals Ordered by Litter Size",
       xlab = "Litter ID (Sorted by ID)",
       ylab = "Studentized Residuals",
       scales = list(
           x = list(
               draw = TRUE,      # Turn the labels back on
               rot = 45,         # Rotate 90 degrees so they don't overlap
               cex = 0.7         # Make text slightly smaller to fit everyone
           )
       ),
       panel = function(x, y, ...) {
           # pch = "|" ensures the median is a horizontal line
           panel.bwplot(x, y, ..., pch = "|")
           
           # Calculate and draw the MEAN as an OPEN diamond (pch = 5)
           m <- aggregate(y ~ x, FUN = mean)
           panel.points(m$x, m$y, pch = 5, col = "black", cex = 1.1, lwd = 1.2)
           
           # Reference lines
           panel.abline(h = 0, lty = 2, col = "gray50")
           panel.abline(h = c(-2, -4, -6, 2), lty = 2, col = "gray50")       },
       par.settings = list(
           # Outliers
           plot.symbol = list(pch = 1, col = "black", cex = 0.6),
           # Box borders
           box.rectangle = list(col = "black", lwd = 1),
           # Whiskers
           box.umbrella = list(col = "black", lty = 1, lwd = 1)
       ))
