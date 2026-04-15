library(ggplot2)
library(dplyr)
library(tidyverse)


# High correlation setting (rho=0.8)
#### Imputation results ####
results_imputation <- read.csv("C:/Users/evebe/OneDrive/Documents/Desktop/MT4599 Project in Mathemaics Statistics/results/imputation_results.csv")

results_imputation$method <- factor(results_imputation$method, levels = c("Simple", "KNN", "MissForest"))
results_imputation$split <- factor(results_imputation$split, levels = c("train", "test"), labels = c("Train", "Test"))

# RMSE violin plot
ggplot(data = results_imputation, aes(x = method, y = rmse, colour = method)) +
  geom_violin(trim = FALSE, linewidth = 0.3) +
  geom_boxplot(width = 0.1, linewidth = 0.3) +
  labs(x = "Imputation method", y = "RMSE", colour = "Imputer") +
  scale_colour_manual(values =  c("Simple" = "#009E73", "KNN" = "#56B4E9", "MissForest" = "#E69F00"))  +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12), legend.title = element_text(size = 14), legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) + 
  facet_grid(split ~ missing_rate)

# MAE violin plot
ggplot(data = results_imputation, aes(x = method, y = mae, colour = method)) +
  geom_violin(trim = FALSE, linewidth = 0.3) +
  geom_boxplot(width = 0.1, linewidth = 0.3) +
  labs(x = "Imputation method", y = "MAE", colour = "Imputer") +
  scale_colour_manual(values =  c("Simple" = "#009E73", "KNN" = "#56B4E9", "MissForest" = "#E69F00"))  +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12), legend.title = element_text(size = 14), legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) + 
  facet_grid(split ~ missing_rate)

# R2 violin plot
ggplot(data = results_imputation, aes(x = method, y = r2, colour = method)) +
  geom_violin(trim = FALSE, linewidth = 0.3) +
  geom_boxplot(width = 0.1, linewidth = 0.3) +
  labs(x = "Imputation method", y = expression(R^2), colour = "Imputer") +
  scale_colour_manual(values =  c("Simple" = "#009E73", "KNN" = "#56B4E9", "MissForest" = "#E69F00"))  +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12), legend.title = element_text(size = 14), legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) + 
  facet_grid(split ~ missing_rate)

# KS statistics box plots
ks_results <- read.csv("C:/Users/evebe/OneDrive/Documents/Desktop/MT4599 Project in Mathemaics Statistics/results/ks_per_feature.csv")
ks_results$method <- factor(ks_results$method, levels = c("simple", "knn", "missforest"), labels = c("Simple", "KNN", "MissForest"))
ks_results$split <- factor(ks_results$split, levels = c("train", "test"), labels = c("Train", "Test"))

ggplot(data = ks_results, aes(x = missing_rate, y = ks)) +
  geom_boxplot(aes(colour = missing_rate), width = 0.5, linewidth = 0.4, outlier.size = 1) +
  facet_grid(split ~ method) +
  ylim(0,1)+
  labs(x = "Missing rate", y = "Kolmogorov–Smirnov statistic", colour = "Missing rate"
  ) +
  scale_colour_manual(
    values = c("10%" = "#005AB5", "25%" = "#DC3220")
  ) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12), legend.title = element_text(size = 14), legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))


#### Classification results ####

results_classification <- read.csv("C:/Users/evebe/OneDrive/Documents/Desktop/MT4599 Project in Mathemaics Statistics/results/classification_results.csv")
results_classification <- results_classification %>% mutate(
  classifier = factor(classifier),
  imputer = factor(imputer, levels = c("simple", "knn", "missforest"), labels = c("Simple", "KNN", "MissForest")),
  missing_rate = factor(missing_rate, levels = c("10%", "25%"), labels = c("Low (10%)", "High (25%)")))

complete_data <- read.csv("C:/Users/evebe/OneDrive/Documents/Desktop/MT4599 Project in Mathemaics Statistics/results/classification_results_complete.csv")
complete_data <- complete_data %>% mutate(
  classifier = factor(classifier, levels = levels(results_classification$classifier)))

# Accuracy density plot
ggplot(results_classification,
       aes(x = accuracy, colour = imputer, linetype = missing_rate)) +
  
  geom_density(linewidth=1.0) +
  geom_density(data = complete_data, aes(x = accuracy, colour = "Complete"), linetype = "longdash", linewidth=1, inherit.aes = FALSE) +
  facet_grid(classifier ~ .) +
  
  scale_colour_manual(values = c("Simple" = "#F0E442","KNN" = "#D55E00","MissForest" = "#CC79A7","Complete" = "#0072B2")) +
  labs(x = "Accuracy",y = "Density",colour = "Imputation Method",linetype = "Missing Rate") +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12), legend.title = element_text(size = 14), legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))


# F1 score density plot
ggplot(results_classification,
       aes(x = f1, colour = imputer, linetype = missing_rate)) +
  
  geom_density(linewidth=1.0) +
  geom_density(data = complete_data, aes(x = f1, colour = "Complete"), linetype = "longdash", linewidth=1, inherit.aes = FALSE) +
  facet_grid(classifier ~ .) +
  
  scale_colour_manual(values = c("Simple" = "#F0E442","KNN" = "#D55E00","MissForest" = "#CC79A7","Complete" = "#0072B2")) +
  labs(x = "F1 score",y = "Density",colour = "Imputation Method",linetype = "Missing Rate") +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12), legend.title = element_text(size = 14), legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))

# ROC-AUC density plot
ggplot(results_classification,
       aes(x = roc_auc, colour = imputer, linetype = missing_rate)) +
  
  geom_density(linewidth=1.0) +
  geom_density(data = complete_data, aes(x = roc_auc, colour = "Complete"), linetype = "longdash", linewidth=1, inherit.aes = FALSE) +
  facet_grid(classifier ~ .) +
  
  scale_colour_manual(values = c("Simple" = "#F0E442","KNN" = "#D55E00","MissForest" = "#CC79A7","Complete" = "#0072B2")) +
  labs(x = "ROC-AUC",y = "Density", colour = "Imputation Method",linetype = "Missing Rate") +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12), legend.title = element_text(size = 14), legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))


# Accuracy violin plot
df <- read.csv("C:/Users/evebe/OneDrive/Documents/Desktop/MT4599 Project in Mathemaics Statistics/results/classification_results.csv")
df <- df %>%
  mutate(
    missing_rate = factor(missing_rate, levels = c("10%", "25%")),
    imputer = recode(imputer, "simple" = "Simple", "knn" = "KNN", "missforest" = "MissForest"),
    imputer = factor(imputer, levels = c("Simple", "KNN", "MissForest")),
    classifier = recode(classifier, "LogisticRegression" = "Logistic Regression", "NeuralNetwork" = "Neural Network", "XGBoost" = "XGBoost"),
    classifier = factor(classifier, levels = c("Logistic Regression", "Neural Network", "XGBoost")),
    
    group = interaction(missing_rate, imputer, sep = ".", lex.order = TRUE),
    group = factor(group, levels = c("10%.Simple", "10%.KNN", "10%.MissForest",
                                     "25%.Simple", "25%.KNN", "25%.MissForest"))
  ) %>%
  filter(
    missing_rate %in% c("10%", "25%"),
    imputer %in% c("Simple", "KNN", "MissForest"),
    classifier %in% c("Logistic Regression", "Neural Network", "XGBoost")
  )

# mean per violin
means <- df %>%
  group_by(classifier, group, missing_rate, imputer) %>%
  summarise(mean_acc = mean(accuracy, na.rm = TRUE), .groups = "drop")

ggplot(df, aes(x = accuracy, y = group, colour = missing_rate, linetype = imputer)) +
  geom_violin(fill = "white", linewidth = 0.8, trim = TRUE) +
  geom_point(data = means, aes(x = mean_acc, y = group, colour = missing_rate), size = 2.5) +
  facet_wrap(~ classifier, nrow = 1) +
  scale_colour_manual(values = c("10%" = "#005AB5", "25%" = "#DC3220"), name = "Missing rate") +
  scale_linetype_manual(values = c("Simple" = "solid", "KNN" = "dashed", "MissForest" = "dotted"), name = "Imputation") +
  labs(x = "Accuracy", y = NULL) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_blank(),
        strip.text = element_text(size = 12), legend.title = element_text(size = 14), legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14), axis.title.y = element_blank())


# F1 score violin plot
df <- read.csv("C:/Users/evebe/OneDrive/Documents/Desktop/MT4599 Project in Mathemaics Statistics/results/classification_results.csv")
df <- df %>%
  mutate(
    missing_rate = factor(missing_rate, levels = c("10%", "25%")),
    
    imputer = recode(imputer, "simple" = "Simple", "knn" = "KNN", "missforest" = "MissForest"),
    imputer = factor(imputer, levels = c("Simple", "KNN", "MissForest")),
    classifier = recode(classifier, "LogisticRegression" = "Logistic Regression", "NeuralNetwork" = "Neural Network", "XGBoost" = "XGBoost"),
    classifier = factor(classifier, levels = c("Logistic Regression", "Neural Network", "XGBoost")),
    
    group = interaction(missing_rate, imputer, sep = ".", lex.order = TRUE),
    group = factor(group, levels = c("10%.Simple", "10%.KNN", "10%.MissForest",
                                     "25%.Simple", "25%.KNN", "25%.MissForest"))
  ) %>%
  filter(
    missing_rate %in% c("10%", "25%"),
    imputer %in% c("Simple", "KNN", "MissForest"),
    classifier %in% c("Logistic Regression", "Neural Network", "XGBoost")
  )

# mean per violin
means <- df %>%
  group_by(classifier, group, missing_rate, imputer) %>%
  summarise(mean_f1 = mean(f1, na.rm = TRUE), .groups = "drop")

ggplot(df, aes(x = f1, y = group, colour = missing_rate, linetype = imputer)) +
  geom_violin(fill = "white", linewidth = 0.8, trim = TRUE) +
  geom_point(data = means, aes(x = mean_f1, y = group, colour = missing_rate), size = 2.5) +
  facet_wrap(~ classifier, nrow = 1) +
  scale_colour_manual(values = c("10%" = "#005AB5", "25%" = "#DC3220"), name = "Missing rate") +
  scale_linetype_manual(values = c("Simple" = "solid", "KNN" = "dashed", "MissForest" = "dotted"), name = "Imputation") +
  labs(x = "F1 score", y = NULL) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_blank(),
        strip.text = element_text(size = 12), legend.title = element_text(size = 14), legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14), axis.title.y = element_blank())

# ROC-AUC violin plot
df <- read.csv("C:/Users/evebe/OneDrive/Documents/Desktop/MT4599 Project in Mathemaics Statistics/results/classification_results.csv")
df <- df %>%
  mutate(
    missing_rate = factor(missing_rate, levels = c("10%", "25%")),
    
    imputer = recode(imputer, "simple" = "Simple", "knn" = "KNN", "missforest" = "MissForest"),
    imputer = factor(imputer, levels = c("Simple", "KNN", "MissForest")),
    classifier = recode(classifier, "LogisticRegression" = "Logistic Regression", "NeuralNetwork" = "Neural Network", "XGBoost" = "XGBoost"),
    classifier = factor(classifier, levels = c("Logistic Regression", "Neural Network", "XGBoost")),
    
    group = interaction(missing_rate, imputer, sep = ".", lex.order = TRUE),
    group = factor(group, levels = c("10%.Simple", "10%.KNN", "10%.MissForest",
                                     "25%.Simple", "25%.KNN", "25%.MissForest"))
  ) %>%
  filter(
    missing_rate %in% c("10%", "25%"),
    imputer %in% c("Simple", "KNN", "MissForest"),
    classifier %in% c("Logistic Regression", "Neural Network", "XGBoost")
  )

# mean per violin
means <- df %>%
  group_by(classifier, group, missing_rate, imputer) %>%
  summarise(mean_rocauc = mean(roc_auc, na.rm = TRUE), .groups = "drop")

ggplot(df, aes(x = roc_auc, y = group, colour = missing_rate, linetype = imputer)) +
  geom_violin(fill = "white", linewidth = 0.8, trim = TRUE) +
  geom_point(data = means, aes(x = mean_rocauc, y = group, colour = missing_rate), size = 2.5) +
  facet_wrap(~ classifier, nrow = 1) +
  scale_colour_manual(values = c("10%" = "#005AB5", "25%" = "#DC3220"), name = "Missing rate") +
  scale_linetype_manual(values = c("Simple" = "solid", "KNN" = "dashed", "MissForest" = "dotted"), name = "Imputation") +
  labs(x = "ROC-AUC", y = NULL) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_blank(),
        strip.text = element_text(size = 12), legend.title = element_text(size = 14), legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14), axis.title.y = element_blank())




# Low correlation setting (rho=0.2)

#### Imputation results ####
results_imputationrho02 <- read.csv("C:/Users/evebe/OneDrive/Documents/Desktop/MT4599 Project in Mathemaics Statistics/results/imputation_results_rho02.csv")
results_imputationrho02$method <- factor(results_imputationrho02$method, levels = c("Simple", "KNN", "MissForest"))
results_imputationrho02$split <- factor(results_imputationrho02$split, levels = c("train", "test"), labels = c("Train", "Test"))

# RMSE violin plot
ggplot(data = results_imputationrho02, aes(x = method, y = rmse, colour = method)) +
  geom_violin(trim = FALSE, linewidth = 0.3) +
  geom_boxplot(width = 0.1, linewidth = 0.3) +
  labs(x = "Imputation method", y = "RMSE", colour = "Imputer") +
  scale_colour_manual(values =  c("Simple" = "#009E73", "KNN" = "#56B4E9", "MissForest" = "#E69F00"))  +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12), legend.title = element_text(size = 14), legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) + 
  facet_grid(split ~ missing_rate)

# MAE violin plot
ggplot(data = results_imputationrho02, aes(x = method, y = mae, colour = method)) +
  geom_violin(trim = FALSE, linewidth = 0.3) +
  geom_boxplot(width = 0.1, linewidth = 0.3) +
  labs(x = "Imputation method", y = "MAE", colour = "Imputer") +
  scale_colour_manual(values =  c("Simple" = "#009E73", "KNN" = "#56B4E9", "MissForest" = "#E69F00"))  +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12), legend.title = element_text(size = 14), legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) + 
  facet_grid(split ~ missing_rate)

# R2 violin plot
ggplot(data = results_imputationrho02, aes(x = method, y = r2, colour = method)) +
  geom_violin(trim = FALSE, linewidth = 0.3) +
  geom_boxplot(width = 0.1, linewidth = 0.3) +
  labs(x = "Imputation method", y = expression(R^2), colour = "Imputer") +
  scale_colour_manual(values =  c("Simple" = "#009E73", "KNN" = "#56B4E9", "MissForest" = "#E69F00"))  +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12), legend.title = element_text(size = 14), legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) + 
  facet_grid(split ~ missing_rate)


# Classification accuracy comparison plot

# High correlation data (rho = 0.8)
results_classification_high <- read.csv("C:/Users/evebe/OneDrive/Documents/Desktop/MT4599 Project in Mathemaics Statistics/results/classification_results.csv")
complete_data_high <- read.csv("C:/Users/evebe/OneDrive/Documents/Desktop/MT4599 Project in Mathemaics Statistics/results/classification_results_complete.csv")

# Low correlation data (rho = 0.2)
results_classification_low <- read.csv("C:/Users/evebe/OneDrive/Documents/Desktop/MT4599 Project in Mathemaics Statistics/results/classification_results_rho02.csv")
complete_data_low <- read.csv("C:/Users/evebe/OneDrive/Documents/Desktop/MT4599 Project in Mathemaics Statistics/results/classification_results_complete_rho02.csv")

# format classification results
format_classification <- function(df, corr_label) {
  df %>%
    mutate(
      classifier = factor(classifier,
        levels = c("LogisticRegression", "NeuralNetwork", "XGBoost"),
        labels = c("Logistic Regression", "Neural Network", "XGBoost")
      ),
      imputer = factor(imputer,
        levels = c("simple", "knn", "missforest"),
        labels = c("Simple", "KNN", "MissForest")
      ),
      missing_rate = factor(missing_rate,
        levels = c("10%", "25%"),
        labels = c("Low (10%)", "High (25%)")
      ),
      correlation = factor(corr_label,
        levels = c("Low correlation (\u03c1 = 0.2)", "High correlation (\u03c1 = 0.8)")
      )
    )
}

# format complete data classification results
format_complete <- function(df, corr_label) {
  df %>%
    mutate(
      classifier = factor(classifier,
        levels = c("LogisticRegression", "NeuralNetwork", "XGBoost"),
        labels = c("Logistic Regression", "Neural Network", "XGBoost")
      ),
      correlation = factor(corr_label,
        levels = c("Low correlation (\u03c1 = 0.2)", "High correlation (\u03c1 = 0.8)")
      )
    )
}

results_classification_high <- format_classification(results_classification_high, "High correlation (\u03c1 = 0.8)")
results_classification_low <- format_classification(results_classification_low, "Low correlation (\u03c1 = 0.2)")

complete_data_high <- format_complete(complete_data_high, "High correlation (\u03c1 = 0.8)")
complete_data_low <- format_complete(complete_data_low, "Low correlation (\u03c1 = 0.2)")

# Combine datasets
results_classification_both <- bind_rows(results_classification_low, results_classification_high)
complete_data_both <- bind_rows(complete_data_low, complete_data_high)

# Combined accuracy density plot
ggplot(results_classification_both, aes(x = accuracy, colour = imputer, linetype = missing_rate)) +
  geom_density(linewidth = 1.0) +
  geom_density(data = complete_data_both, aes(x = accuracy, colour = "Complete"),
    linetype = "longdash", linewidth = 1, inherit.aes = FALSE) +
  facet_grid(classifier ~ correlation) +
  scale_colour_manual(
    values = c("Simple" = "#F0E442", "KNN" = "#D55E00", "MissForest" = "#CC79A7", "Complete" = "#0072B2")) +
  labs(x = "Accuracy", y = "Density", colour = "Imputation Method", linetype = "Missing Rate") +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), strip.text = element_text(size = 12),
        legend.title = element_text(size = 14), legend.text = element_text(size = 12), axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14), legend.position = "right")

