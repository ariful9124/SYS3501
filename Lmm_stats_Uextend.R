getwd()

#install.packages("lme4")
#install.packages("lmerTest")  # Provides p-values for mixed models
#install.packages("ggplot2")   # For visualization (optional)
#install.packages("emmeans")

data<- read.csv("total_df_stat_domhand_R_4removed.csv")


library(lme4)        # For mixed-effects modeling
library(lmerTest)    # For p-values in mixed-effects models
library(emmeans)
library(ggplot2)

# Mixed-effects model
model <- lmer(Median_jerk_metric ~ Patient_Type + (1|Patient_Id), data = data)

# Summary of the model
summary(model)




# Post-hoc pairwise comparisons
posthoc <- emmeans(model, pairwise ~ Patient_Type)
summary(posthoc)

# Optionally, write the result to a CSV file
#write.csv(significant_features_df, "significant_features.csv", row.names = TRUE)
#columns <- c("Patient_Id", "date", "Scaling_factor", "Mean_Duration", "Median_Duration", 
             "Std_Duration", "Max_Duration", "Perc10_Duration", "Perc90_Duration", 
             "Mean_Log_Mean", "Median_Log_Mean", "Std_Log_Mean", "Max_Log_Mean", 
             "Perc10_Log_Mean", "Perc90_Log_Mean", "Mean_Log_Dis", "Median_Log_Dis", 
             "Std_Log_Dis", "Max_Log_Dis", "Perc10_Log_Dis", "Perc90_Log_Dis", 
             "Ave_num_subm_per_s", "Mean_jerk_metric", "Median_jerk_metric", 
             "Std_jerk_metric", "Mean_speed_metric", "Median_speed_metric", 
             "Std_speed_metric", "PC1_mean", "PC1_std", "PC1_kurtosis", "PC1_skewness", 
             "PC2_mean", "PC2_std", "PC2_kurtosis", "PC2_skewness", "PC3_mean", 
             "PC3_std", "PC3_kurtosis", "PC3_skewness", "PC4_mean", "PC4_std", 
             "PC4_kurtosis", "PC4_skewness", "PC5_mean", "PC5_std", "PC5_kurtosis", 
             "PC5_skewness", "Patient_Type")

#feature_columns <- columns[!(columns %in% c("Patient_Id", "date", "Patient_Type"))]#
#ggplot(mtcars, aes(x=as.factor(cyl), y=mpg)) + 
#  geom_boxplot(fill="slateblue", alpha=0.2) + 
 # xlab("cyl")

# shade of green #28B463
# shade of orange #FF5733
# Scaling_factor, Ave_num_subm_per_s, PC1_skewness, PC5_skewness, Perc90_Log_Mean

ggplot(data, aes(x = Patient_Type, y = Mean_Log_Mean)) +
  geom_boxplot(fill="#28B463", alpha=0.2, size=0.9) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 24),  # Bold and center title
        axis.title.x = element_text(face = "bold", size = 32),  # Bold x-axis label
        axis.title.y = element_text(face = "bold", size = 24),
        axis.text.x = element_text(face = "bold", size = 24)) +  # Remove minor grid lines
  labs(title = "Boxplot of Mean_Log_Mean by Cohort", x = "Cohort", y = "Mean_Log_Mean")

# Function to identify outliers using IQR method
remove_outliers_iqr <- function(df, feature) {
  Q1 <- quantile(df[[feature]], 0.25)
  Q3 <- quantile(df[[feature]], 0.75)
  IQR <- Q3 - Q1
  
  # Define outlier boundaries
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Filter the dataset
  df_clean <- df[df[[feature]] >= lower_bound & df[[feature]] <= upper_bound, ]
  return(df_clean)
}

# Apply the function to each feature you want to check for outliers

data_sample <- remove_outliers_iqr(data, "PC1_skewness")



