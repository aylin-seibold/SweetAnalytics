#----------------------------------#
#### Structure of this code ########
#----------------------------------#
# 1. ANOVA Test for Ingredient Influence on Winpercent
# 2. Random Forest Model
# 3. Linear Models
# 4. Generalized Linear Model (GLM) for Top 30% Candies
# 5. GLM for Bottom 30% Candies
# 6. Interaction Models
#----------------------------------#

#----------------------------------#
#### 1. ANOVA Test for Ingredient Influence on Winpercent ####
#----------------------------------#

# Function to perform Shapiro-Wilk test for normality on each category
shapiro.test.by.category <- function(data, categorical.vars, target.var) {
  results <- list()
  for (var in categorical.vars) {
    for (level in unique(data[[var]])) {
      subset.data <- data[get(var) == level, ..target.var, drop = FALSE]
      test.result <- shapiro.test(subset.data[[target.var]])
      results[[paste(var, level, sep = "_")]] <- test.result$p.value > 0.05
    }
  }
  
  # Convert the results list to a DataFrame to show the normality assumption for each category
  results.df <- data.frame(
    variable_level = names(results),
    normality_assumed = unlist(results)
  )
  return(results.df)
}

# Specify categorical variables that need to be tested
categorical.vars <- features

# Define the target variable (the one to check for normality)
target.var <- "win.prop"

# Perform Shapiro-Wilk test for each category and variable
shapiro.results <- shapiro.test.by.category(candy.data, categorical.vars, target.var)

# Print the results of the Shapiro-Wilk test (whether normality is assumed)
shapiro.results

# Perform ANOVA (Analysis of Variance) to check if there are significant differences
# in win.prop (win percentage) based on ingredients (categorical variables)
anova.model <- aov(win.prop ~ chocolate + fruity + caramel + peanutyalmondy + 
                     nougat + crispedricewafer + hard + bar + pluribus, 
                   data = candy.data)
summary(anova.model)

#----------------------------------#
#### 2. Random Forest Model ####
#----------------------------------#

# Train a Random Forest model to analyze the impact of ingredients on win.prop
# Random Forest is used to predict the target variable (win.prop) using the categorical variables as predictors
model.rf <- randomForest(win.prop ~ chocolate + fruity + caramel + peanutyalmondy + 
                           nougat + crispedricewafer + hard + bar + pluribus, 
                         data = candy.data)

# Display the Random Forest model's summary
# This will show information about the training process and overall performance
model.rf

# Display the importance of each feature (ingredient) in the Random Forest model
# This shows which ingredients contribute the most to predicting win.prop
importance(model.rf)

#----------------------------------#
#### 3. Linear Models ####
#----------------------------------#

# Fit a linear regression model to predict win.prop (win percentage) based on ingredients and other features
model.lm <- lm(win.prop ~ chocolate + hard + fruity + peanutyalmondy + caramel + 
                 nougat + crispedricewafer + bar + pluribus + sugarpercent + 
                 pricepercent, data = candy.data)
summary(model.lm)

#----------------------------------#
#### 4. Generalized Linear Model (GLM) for Top 30% Candies ####
#----------------------------------#

# Fit a GLM model to predict the probability of being in the top 30% based on ingredients and other features
# The "binomial" family is used as the outcome is binary (Top 30% or not)
model.glm.top <- glm(top30 ~ chocolate + hard + fruity + peanutyalmondy + caramel + 
                       nougat + crispedricewafer + bar + pluribus + sugarpercent + 
                       pricepercent, family = "binomial", data = candy.data)
summary(model.glm.top)

#----------------------------------#
#### 5. GLM for Bottom 30% Candies ####
#----------------------------------#

# Fit a GLM model to predict the probability of being in the bottom 30% based on ingredients and other features
# Again, using the "binomial" family for a binary outcome
model.glm.bottom <- glm(bottom30 ~ chocolate + hard + fruity + peanutyalmondy + caramel + 
                          nougat + crispedricewafer + bar + pluribus + sugarpercent + 
                          pricepercent, family = "binomial", data = candy.data)
summary(model.glm.bottom)

#----------------------------------#
#### 6. Interaction Models ####
#----------------------------------#

# Fit a linear regression model to predict win.prop (win percentage) based on interactions of ingredients

# Print Top 5 candies
top.30[1:5]

# Linear model with interaction effect between chocolate and peanutyalmondy
model.lm.interaction1 <- lm(win.prop ~ chocolate*peanutyalmondy, data = candy.data)
summary(model.lm.interaction1)

# Linear model with interaction effect between chocolate and bar
model.lm.interaction2 <- lm(win.prop ~ chocolate*bar, data = candy.data)
summary(model.lm.interaction2)

# Linear model with interaction effect between chocolate and crispedricewafer
model.lm.interaction3 <- lm(win.prop ~ chocolate*crispedricewafer, data = candy.data)
summary(model.lm.interaction3)
