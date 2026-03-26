source("b_exploration.R")


# Later diagnostic plot material

# This plots the "Pure" effect of SES from your LME model
plot(effect("ses", model.5.ml.fit), 
     main="Effect of SES on Math Gain (Controlled)",
     ylab="Predicted Math Gain",
     xlab="Socioeconomic Status")
