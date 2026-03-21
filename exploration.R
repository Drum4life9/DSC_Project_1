rm(list = ls())
load("classroom6.RData")

data = classroom6

library(dplyr)
library(knitr)
library(ggplot2)
library(lattice)


# Make factors easier to understand
data$sex <- factor(data$sex, 
                   levels = c(0, 1), 
                   labels = c("male", "female"))
data$minority <- factor(data$minority, 
                        levels = c(0, 1), 
                        labels = c("N", "Y"))

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
    summarise(
        N_Obs = n(),
        mean_mathkind = mean(mathkind, na.rm = TRUE),
        sd_mathkind = sd(mathkind, na.rm = TRUE),
        mean_yearstea = mean(yearstea, na.rm = TRUE),
        sd_yearstea = sd(yearstea, na.rm = TRUE)
    )

print(kable(analysis_table_lvl_2, 
            col.names = c("N",
                          "mean_yearstea", "sd_yearstea", 
                          "mean_mathprep", "sd_mathprep"), 
            digits = 2, 
            caption = "          Analysis Variable : LVL 2 factors\n----------------------------------------------------------------"))



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
            caption = "             Analysis Variable : MathGain\n----------------------------------------------------------------"))

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
            caption = "             Analysis Variable : MathGain\n----------------------------------------------------------------"))

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
            caption = "             Analysis Variable : MathGain\n----------------------------------------------------------------"))


# ---------------------------------------------------BW plots----------------------------------------------------------------------------


# Boxplot by sex
bwplot(mathgain ~ sex, 
       data = data,
       layout = c(1, 1), 
       main = "Figure 3.1 with Pronounced Median Lines",
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
       main = "Figure 3.1 with Pronounced Median Lines",
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
       main = "Figure 3.1 with Pronounced Median Lines",
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
    labs(title = "Math Gain by SES and Minority",
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

# ------------------ YearStea -----------------------

# MathGain ~ YearStea
ggplot(data, aes(x = yearstea, y = mathgain, color = classid)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "red") +       
    labs(title = "Relationship between YearStea and Math Gain",
         x = "SES",
         y = "Gain in Math Score") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    theme_minimal()


# Colored by sex
ggplot(data, aes(x = yearstea, y = mathgain, color = sex)) +
    geom_point(alpha = 0.6) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_smooth(method = "lm", se = FALSE) + 
    labs(title = "Math Gain by YearStea and Sex",
         x = "YearStea",
         y = "Gain in Math Score",
         color = "Student Sex") +
    theme_minimal()

# Colored by minority
ggplot(data, aes(x = yearstea, y = mathgain, color = minority)) +
    geom_point(alpha = 0.6) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_smooth(method = "lm", se = FALSE) + 
    labs(title = "Math Gain by YearStea and Minority",
         x = "YearStea",
         y = "Gain in Math Score",
         color = "Student Minority") +
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
    labs(title = "Math Gain by SES and Minority",
         x = "MathPrep",
         y = "Gain in Math Score",
         color = "Student Minority") +
    theme_minimal()

