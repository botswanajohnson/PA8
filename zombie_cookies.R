# zombie_cookies.R
# Botswana Johnson
# 10/29/2026
# analyze zombie cookie recipes using one-way ANOVA

# create the data for each cookie recipe
RecipeA <- c(0, 4, 4, 5, 4, 1, 2, 3, 3, 5)
RecipeB <- c(7, 6, 4, 2, 3, 0, 8, 6, 2, 3)
RecipeC <- c(1, 0, 1, 0, NA, 1, 1, NA, 1, 0)

# combine the recipe data into a data frame
df <- data.frame(RecipeA, RecipeB, RecipeC)

df

# stack the data and remove missing values
newdf <- na.omit(stack(df))

newdf

# summarize the stacked data
initial_summary <- summary(newdf)

initial_summary

# response: transformation significance values
# predictor: cookie recipe
# H0: the mean transformation values are equal for all recipes
# HA: the mean transformation values are not all equal

# run the one-way ANOVA test
anova_test <- aov(values ~ ind, data = newdf)

# summarize the ANOVA results
anova_summary <- summary(anova_test)

anova_summary

# get the p-value
pval <- anova_summary[[1]]$`Pr(>F)`[1]

pval

# determine if the null hypothesis should be rejected
if (pval < 0.05) {
  cat("Sufficient evidence to reject the null hypothesis.\n")
  cat("Ingredient X may have an effect on the results.\n")
} else {
  cat("Not enough evidence to reject the null hypothesis.\n")
}