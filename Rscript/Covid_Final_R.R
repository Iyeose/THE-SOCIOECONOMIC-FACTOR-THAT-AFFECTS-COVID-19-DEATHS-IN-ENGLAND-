#-----Section 01-------------------------------------------

# set working directory
setwd(dirname(file.choose()))
getwd()

# read in data from csv file
Covid <- read.csv("Covid19_standardized_data.csv", stringsAsFactors = FALSE)
head(Covid)
str(Covid)

# select dependent and independent variables
Covid_1 <- data.frame(Covid$Total_death_sd,Covid$Adults_sd,Covid$Children_sd,Covid$Ederly_sd,Covid$Pre_ederly_sd,Covid$Youth_sd,Covid$four_plus_bedrooms_sd,Covid$one_bedroom_sd,Covid$three_bedrooms_sd,Covid$two_bedrooms_sd,Covid$Female_sd,Covid$Male_sd,Covid$Bad_health_sd,Covid$Good_health_sd,Covid$Verybad_health_sd,Covid$Verygood_health_sd,Covid$Four_dimension_deprivation_sd,Covid$No_dimension_deprivation_sd,Covid$One_dimension_deprivation_sd,Covid$Three_dimension_deprivation_sd,Covid$Two_dimension_deprivation_sd,Covid$High_level_qualifications_sd,Covid$Low_level_qualifications_sd,Covid$No_qualifications_sd,Covid$Other_qualifications_sd)

# Rename columns
colnames(Covid_1) <- c('Total_death_sd','Adults_sd','Children_sd','Ederly_sd','Pre_ederly_sd','Youth_sd','four_plus_bedrooms_sd','one_bedroom_sd','three_bedrooms_sd','two_bedrooms_sd','Female_sd','Male_sd','Bad_health_sd','Good_health_sd','Verybad_health_sd','Verygood_health_sd','Four_dimension_deprivation_sd','No_dimension_deprivation_sd','One_dimension_deprivation_sd','Three_dimension_deprivation_sd','Two_dimension_deprivation_sd','High_level_qualifications_sd','Low_level_qualifications_sd','No_qualifications_sd','Other_qualifications_sd')



# Generate plots and save to working directory

library(ggplot2)
library(car)

for (var in colnames(Covid_1)) {
  # Histogram with density curve
  png(paste0(var, "_histogram_density.png"))
  hist(
    Covid_1[[var]], 
    main = paste("Histogram of", var), 
    xlab = var, 
    col = "lightblue", 
    border = "black", 
    prob = TRUE # This scales the histogram to show density
  )
  
  # Add density curve
  lines(
    density(Covid_1[[var]], na.rm = TRUE), 
    col = "red", 
    lwd = 2
  )
  dev.off()
  
  # Box Plot
  png(paste0(var, "_boxplot.png"))
  boxplot(Covid_1[[var]], main = paste("Box Plot of", var), ylab = var, col = "lightgreen")
  dev.off()
  
  # Q-Q Plot
  png(paste0(var, "_qqplot.png"))
  qqPlot(Covid_1[[var]], main = paste("Q-Q Plot of", var), col = "blue")
  dev.off()
}

# Store p-values for tests
ks_results <- numeric()
sw_results <- numeric()
variables <- colnames(Covid_1)

for (var in variables) {
  data <- Covid_1[[var]]
  
  # Kolmogorov-Smirnov Test
  ks_test <- ks.test(data, "pnorm", mean(data, na.rm = TRUE), sd(data, na.rm = TRUE))
  ks_results <- c(ks_results, ks_test$p.value)
  
  # Shapiro-Wilk Test
  sw_test <- shapiro.test(data)
  sw_results <- c(sw_results, sw_test$p.value)
}

# Combine results into a data frame
results_table <- data.frame(
  Variable = variables,
  KS_p_value = ks_results,
  KS_Normal = ifelse(ks_results > 0.05, "Yes", "No"),
  SW_p_value = sw_results,
  SW_Normal = ifelse(sw_results > 0.05, "Yes", "No")
)

# Print the results table
print(results_table)

# Create a dataframe for storing results
Covid_2 <- data.frame(Variable = character(), Correlation = numeric(), stringsAsFactors = FALSE)

# Define dependent variable
dependent_var <- Covid$Total_death_sd

# Get the list of independent variables
independent_vars <- names(Covid_1)[!names(Covid_1) %in% "Total_death_sd"]

# Loop through each independent variable
for (var in independent_vars) {
  # Extract the independent variable
  independent_var <- Covid_1[[var]]
  
  # Calculate Spearman's correlation
  correlation <- cor(dependent_var, independent_var, method = "spearman", use = "complete.obs")
  
  # Append results to the dataframe
  Covid_2 <- rbind(Covid_2, data.frame(Variable = var, Correlation = correlation))
}

print(Covid_2)

# Filter variables based on correlation greater than 0.2
filtered <- Covid_2[Covid_2$Correlation > 0.2 | Covid_2$Correlation < -0.2, ]

# Print the filtered results
print(filtered)

library(corrplot)

# Compute correlation matrix including the dependent variable
cor_matrix <- cor(Covid_1, use = "complete.obs", method = "spearman")  # Pearson correlation

# Plot correlation matrix
png("correlation_plot_with_all_variables", width = 2000, height = 2000)
corrplot(
  cor_matrix, 
  method = "color",        # Visual representation method (color blocks)
  type = "upper",          # Show only the upper triangle of the matrix
  tl.col = "black",        # Text label color
  tl.srt = 45,             # Text label rotation
  addCoef.col = "black",   # Add correlation coefficients on the plot
  number.cex = 0.7,        # Font size of coefficients
  col = colorRampPalette(c("red", "white", "blue"))(200) # Color palette
)
dev.off()

library(psych)  
library(factoextra)  

# Subset the data for selected variables
pca_data <- Covid[, c("Children_sd","three_bedrooms_sd","two_bedrooms_sd","Male_sd","Bad_health_sd","Good_health_sd","Verybad_health_sd","Verygood_health_sd","No_dimension_deprivation_sd","Three_dimension_deprivation_sd","Two_dimension_deprivation_sd","High_level_qualifications_sd","Low_level_qualifications_sd","No_qualifications_sd","Other_qualifications_sd")]
# Ensure there are no missing values
pca_data <- na.omit(pca_data)

# KMO Test
kmo_test <- KMO(pca_data)
print(kmo_test)

# Compute Eigenvalues
eigen_values <- eigen(cor(pca_data))$values
print(eigen_values)

# Scree Plot
fviz_eig(prcomp(pca_data, scale. = TRUE), addlabels = TRUE, ylim = c(0, max(eigen_values) + 1))

# Cumulative Scree Plot
cumulative_variance <- cumsum(eigen_values / sum(eigen_values))
plot(cumulative_variance, type = "b", xlab = "Number of Factors", ylab = "Cumulative Variance Explained", 
     main = "Cumulative Scree Plot", ylim = c(0, 1))

abline(h = 0.8, col = "red", lty = 2)  # 80% variance cutoff
abline(h = 0.9, col = "red", lty = 2)  # 90% variance cutoff
abline(h = 0.95, col = "red", lty = 2)  # 95% variance cutoff

# Perform PCA (Varimax Rotation) with 4 Factors
pca_results <- principal(pca_data, nfactors = 4, rotate = "varimax", scores = TRUE)

# Print PCA Output
print(pca_results)

# Subset the data for independent variables (same as PCA variables)
regression_data <- Covid[, c("Children_sd","three_bedrooms_sd","two_bedrooms_sd","Male_sd","Bad_health_sd","Good_health_sd","Verybad_health_sd","Verygood_health_sd","No_dimension_deprivation_sd","Three_dimension_deprivation_sd","Two_dimension_deprivation_sd","High_level_qualifications_sd","Low_level_qualifications_sd","No_qualifications_sd","Other_qualifications_sd")]

# Ensure there are no missing values
regression_data <- na.omit(regression_data)

# Combine the dependent variable with the independent variables
final_data <- data.frame(dependent_var, regression_data)

# Fit the multiple linear regression model
model <- lm(dependent_var ~ ., data = final_data)

# Print the model summary
summary(model)

# Check for multicollinearity using VIF (Variance Inflation Factor)
vif_values <- vif(model)

sqrt(vif(model)) > 2  # if > 2 vif too high

# Subset the data for independent variables (same as PCA variables)
regression_data_1 <- Covid[, c("two_bedrooms_sd","No_dimension_deprivation_sd","Three_dimension_deprivation_sd")]
# Ensure there are no missing values
regression_data_1 <- na.omit(regression_data_1)

# Combine the dependent variable with the independent variables
final_data <- data.frame(dependent_var, regression_data_1)

# Fit the multiple linear regression model
model_1 <- lm(dependent_var ~ ., data = final_data)

# Print the model summary
summary(model_1)

# Check for multicollinearity using VIF (Variance Inflation Factor)
vif_values <- vif(model_1)

sqrt(vif(model_1)) > 2  # if > 2 vif too high

