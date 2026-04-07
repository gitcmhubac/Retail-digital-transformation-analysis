#Install and load package
#install.packages("car")
#install.packages("ggcorrplot")
#install.packages("Hmisc")
#install.packages("cowplot")
#install.packages("pander")
#install.packages("openxlsx")
#install.packages("pracma")
#install.packages("lmtest")
#install.packages("sandwich")
#install.packages("robustbase")
#install.packages("plm")
#install.packages("effects")
#install.packages("moments")
#install.packages("stargazer")
#install.packages("whitestrap")
library(whitestrap)
library(stargazer)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(car)
library(ggcorrplot)
library(Hmisc)
library(gridExtra)
library(patchwork)
library(cowplot)
library(pander)
library(openxlsx)
library(lmtest)
library(sandwich)
library(robustbase)
library(plm)
library(effects)
library(moments)

#Import data
data <- read.csv("C:/Users/Christine/Desktop/Panel Data.csv")
d <- data

#change column name
colnames(data) <- c("id", "company", "year", 
                    "profit_margin_percentage",	"ROA_percentage",	"ROE_percentage", "ratio_intangible_assets_total_assets", "ratio_intangible_assets_on_total_operating_revenue",
                    "number_of_patent", "costs_per_unit_of_revenue",	"ratio_of_fixed_assets",
                    "equity_to_asset_percentage", "debt_to_asset_percentage",	"business_size",	"total_debt_to_total_equity_percentage",	
                    "PE_ratio",	"interest_cover_ratio",	"dividend_yield_ratio_percentage",	"operating_revenue_per_employee_th",
                    "costs_of_employees_operating_revenue_percentage")


#check missing values
data[, 4:20] <- lapply(data[, 4:20], function(x) replace(x, x == "n.a", NA)) #change n.a to NA for counting
data %>% summarise(across(4:20, ~sum(is.na(.)))) #count
data[, 4:20] <- lapply(data[, 4:20], as.numeric) #change to numeric

#replace missing values & detect outliers by percentile
medians <- data %>% summarise(across(4:20, median, na.rm = TRUE))
data <- data %>%
  mutate(across(4:20, ~replace(., is.na(.) | . < quantile(., 0.01, na.rm = TRUE) | . 
                               > quantile(., 0.99, na.rm = TRUE), medians[[cur_column()]])))

#change number of patents to log version
data$number_of_patent <- ifelse(data$number_of_patent > 0, log(data$number_of_patent), 0)

#descriptive statistics
des_stat_data <- data.frame(data[, 3:20])
summary_stat_data <- data.frame(
  Obs = sapply(des_stat_data, function(x) length(na.omit(x))),
  Mean = colMeans(des_stat_data),
  Median = sapply(des_stat_data, median),
  Std = sapply(des_stat_data, sd),
  Min = sapply(des_stat_data, min),
  Max = sapply(des_stat_data, max))
view(summary_stat_data )

write.csv(summary_stat_data, file = "des_stats.csv", row.names = TRUE)

#check skewness
skewness <- skewness(data[, 4:20])

write.csv(skewness, file = "skewness.csv", row.names = TRUE)

#EDA
#create a trend graph of DVs
data$year <- as.factor(data$year)

#create a table to store avg values of DVs
avg_data <- data %>%
  group_by(year) %>%
  summarise(mean_pm = mean(profit_margin_percentage),
            mean_roa = mean(ROA_percentage),
            mean_roe = mean(ROE_percentage))

#point out min and max avg values
max(avg_data$mean_pm)
avg_data$year[which.max(avg_data$mean_pm)]
min(avg_data$mean_pm)
avg_data$year[which.min(avg_data$mean_pm)]

max(avg_data$mean_roa)
avg_data$year[which.max(avg_data$mean_roa)]
min(avg_data$mean_roa)
avg_data$year[which.min(avg_data$mean_roa)]

max(avg_data$mean_roe)
avg_data$year[which.max(avg_data$mean_roe)]
min(avg_data$mean_roe)
avg_data$year[which.min(avg_data$mean_roe)]

#create trend graph
trend_graph <- ggplot(data = avg_data, aes(x = year)) +
  geom_point(aes(y = mean_pm, color = "Profit Margin"), size = 3) +
  geom_point(aes(y = mean_roa, color = "ROA"), size = 3) +
  geom_point(aes(y = mean_roe, color = "ROE"), size = 3) +
  geom_line(aes(y = mean_pm, group = 1, color = "Profit Margin"), linetype = "solid") +
  geom_line(aes(y = mean_roa, group = 1, color = "ROA"), linetype = "solid") +
  geom_line(aes(y = mean_roe, group = 1, color = "ROE"), linetype = "solid") +
  scale_color_manual(values = c("Profit Margin" = "purple", "ROA" = "orange", "ROE" = "deepskyblue"),
                     labels = c("Profit Margin", "ROA", "ROE")) + 
  labs(title = "",
       x = "Year", y = "Mean Value (%)") +
  theme_minimal() +labs(color = "") 

trend_graph 

#boxplots
variable_names <- c("profit_margin_percentage",	"ROA_percentage",	"ROE_percentage", "ratio_intangible_assets_total_assets", "ratio_intangible_assets_on_total_operating_revenue",
                    "number_of_patent", "costs_per_unit_of_revenue", "ratio_of_fixed_assets","equity_to_asset_percentage", "debt_to_asset_percentage",	"business_size",	
                    "total_debt_to_total_equity_percentage",	"PE_ratio",	"interest_cover_ratio",	"dividend_yield_ratio_percentage",	
                    "operating_revenue_per_employee_th", "costs_of_employees_operating_revenue_percentage")

new_variable_names <- c("PM (%)",
                        "ROA (%)", "ROE (%)",
                        "DT1",
                        "DT2", "NP", "OCR",
                        "FAR", "ETA (%)", "DTA (%)",
                        "SIZE", "DTE (%)",
                        "PE", "INT",
                        "DIV (%)", "ORPE ('000)", "CEOR (%)")

boxplots_list <- lapply(seq_along(variable_names), function(i) {
  ggplot(data, aes(x = "", y = .data[[variable_names[i]]])) +
    geom_boxplot() +
    labs(x = NULL, y = new_variable_names[i]) +
    theme_minimal() + theme(text = element_text(size = 10)) +
    theme(axis.text.x = element_blank())  # Remove x-axis labels
})

#combine the boxplots into a single plot
combined_boxplots <- cowplot::plot_grid(plotlist = boxplots_list, ncol = 4)

combined_boxplots

#histograms
p4 <- ggplot(data, aes(x = profit_margin_percentage)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  labs(x = "Profit Margin (%)", y = "Frequency") +
  theme_minimal() + theme(text = element_text(size = 13))

p5 <- ggplot(data, aes(x = ROA_percentage)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  labs(x = "ROA (%)", y = "Frequency") +
  theme_minimal() + theme(text = element_text(size = 13))

p6 <- ggplot(data, aes(x = ROE_percentage)) +
  geom_histogram(binwidth = 50, fill = "steelblue", color = "white") +
  labs(x = "ROE (%)", y = "Frequency") +
  theme_minimal() + theme(text = element_text(size = 13))

p7 <- ggplot(data, aes(x = ratio_intangible_assets_total_assets)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "Ratio Intangible Assets on Total Assets", y = "Frequency") +
  theme_minimal() + theme(text = element_text(size = 13))

p8 <- ggplot(data, aes(x = ratio_intangible_assets_on_total_operating_revenue)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "Ratio of Intangible Assets on Total Operating Revenue", y = "Frequency") +
  theme_minimal() + theme(text = element_text(size = 13))

p9 <- ggplot(data, aes(x = number_of_patent)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  labs(x = "Numbers of Patent", y = "Frequency") +
  theme_minimal() + theme(text = element_text(size = 13))

p10 <- ggplot(data, aes(x = ratio_of_fixed_assets)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "white") +
  labs(x = "Ratio of Fixed Assets", y = "Frequency") +
  theme_minimal() + theme(text = element_text(size = 13))

p11 <- ggplot(data, aes(x = equity_to_asset_percentage)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  labs(x = "Equity to Asset (%)", y = "Frequency") +
  theme_minimal() + theme(text = element_text(size = 13))

p12 <- ggplot(data, aes(x = debt_to_asset_percentage)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  labs(x = "Debt to Asset (%)", y = "Frequency") +
  theme_minimal() + theme(text = element_text(size = 13))

p13 <- ggplot(data, aes(x = business_size)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "Business Size", y = "Frequency") +
  theme_minimal() + theme(text = element_text(size = 13))

p14 <- ggplot(data, aes(x = total_debt_to_total_equity_percentage)) +
  geom_histogram(binwidth = 100, fill = "steelblue", color = "white") +
  labs(x = "Total Debt to Total Equity (%)", y = "Frequency") +
  theme_minimal() + theme(text = element_text(size = 13))

p15 <- ggplot(data, aes(x = PE_ratio)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  labs(x = "PE Ratio", y = "Frequency") +
  theme_minimal() + theme(text = element_text(size = 13))

p16 <- ggplot(data, aes(x = interest_cover_ratio)) +
  geom_histogram(binwidth = 50, fill = "steelblue", color = "white") +
  labs(x = "Interest Cover Ratio", y = "Frequency") +
  theme_minimal() + theme(text = element_text(size = 13))

p17 <- ggplot(data, aes(x = dividend_yield_ratio_percentage)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "Dividend Yield Ratio (%)", y = "Frequency") +
  theme_minimal() + theme(text = element_text(size = 13))

p18 <- ggplot(data, aes(x = operating_revenue_per_employee_th)) +
  geom_histogram(binwidth = 50, fill = "steelblue", color = "white") +
  labs(x = "Operating Revenue per Employee ('000)", y = "Frequency") +
  theme_minimal() + theme(text = element_text(size = 13))

p19 <- ggplot(data, aes(x = costs_of_employees_operating_revenue_percentage)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(x = "Costs of Employees Operating Revenue (%)", y = "Frequency") +
  theme_minimal() + theme(text = element_text(size = 13))

p20 <-ggplot(data, aes(x = costs_per_unit_of_revenue)) +
  geom_histogram(binwidth = 100, fill = "steelblue", color = "white") +
  labs(x = "Op. Costs to Rev.", y = "Frequency") +
  theme_minimal() + theme(text = element_text(size = 13))

p4_20 <- p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + p13 + p14 + p15 + p16 + p17 + p18 + p19 + p20
p4_20


#create cor.matrix table with significance levels
cor_matrix <- cor(data[, 3:20])

#format correlation coefficients with significance indicators
format_corr <- function(x) {
  stars <- ifelse(abs(x) > 0.05, "***", ifelse(abs(x) > 0.01, "**", ifelse(abs(x) > 0.001, "*", "")))
  paste0(format(round(x, 3), nsmall = 3), stars)
}

cor_matrix_formatted <- apply(cor_matrix, 2, format_corr)
print(cor_matrix_formatted, quote = FALSE)

cor_df <- as.data.frame(cor_matrix_formatted)
row.names(cor_df) <- colnames(cor_matrix)

# Save the data frame to an Excel file
write.xlsx(cor_df, "correlation_matrix.xlsx")

#replace outliers
replace_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_threshold <- q1 - 1.5 * iqr
  upper_threshold <- q3 + 1.5 * iqr
  x_outliers <- x < lower_threshold | x > upper_threshold
  x[x_outliers] <- median(x, na.rm = TRUE) 
  return(x)
}

data[, 4:20] <- data[, 4:20] %>% mutate(across(.fns = replace_outliers))

#log version
data$ratio_intangible_assets_total_assets <- ifelse(data$ratio_intangible_assets_total_assets > 0, log(data$ratio_intangible_assets_total_assets), data$ratio_intangible_assets_total_assets)
data$ratio_intangible_assets_on_total_operating_revenue <- ifelse(data$ratio_intangible_assets_on_total_operating_revenue > 0, log(data$ratio_intangible_assets_on_total_operating_revenue), data$ratio_intangible_assets_on_total_operating_revenue)
data$ratio_of_fixed_assets <- ifelse(data$ratio_of_fixed_assets > 0, log(data$ratio_of_fixed_assets), data$ratio_of_fixed_assets)

print(summary_stats)


#normalisation
normalised_data <- function(x) {
  median_val <- median(x, na.rm = TRUE)
  mad_val <- mad(x, na.rm = TRUE)
  if (mad_val > 0) {
    (x - median_val) / mad_val
  } else {
    x
  }
}

data[, 4:20] <- apply(data[, 4:20], 2, normalised_data)

#square root transformation
data[, 4:20] <- data[, 4:20] %>% 
  mutate(across(.cols = -c(ratio_intangible_assets_total_assets, ratio_intangible_assets_on_total_operating_revenue,
                           number_of_patent, PE_ratio, dividend_yield_ratio_percentage, 
                           operating_revenue_per_employee_th, costs_of_employees_operating_revenue_percentage), 
                .fns = function(x) sqrt(abs(x))))

#try to run baseline model 
#create 9 dummy variables for 10 years
year_dummies <- model.matrix(~ factor(year) - 1, data = data)

#LM
baseline_model1 <- lm(profit_margin_percentage	~ ratio_intangible_assets_total_assets + ratio_intangible_assets_on_total_operating_revenue +
                        ratio_of_fixed_assets + equity_to_asset_percentage +
                        debt_to_asset_percentage + business_size +
                        total_debt_to_total_equity_percentage + PE_ratio +
                        interest_cover_ratio + dividend_yield_ratio_percentage +
                        operating_revenue_per_employee_th +
                        costs_of_employees_operating_revenue_percentage + costs_per_unit_of_revenue + year_dummies[, -1],
                      data = data)


baseline_model2 <- lm(ROA_percentage	~ ratio_intangible_assets_total_assets +
                        ratio_of_fixed_assets + equity_to_asset_percentage +
                        debt_to_asset_percentage + business_size +
                        total_debt_to_total_equity_percentage + PE_ratio +
                        interest_cover_ratio + dividend_yield_ratio_percentage +
                        operating_revenue_per_employee_th +
                        costs_of_employees_operating_revenue_percentage + costs_per_unit_of_revenue + year_dummies[, -1],
                      data = data)

baseline_model3 <- lm(ROA_percentage	~ ratio_intangible_assets_on_total_operating_revenue +
                        ratio_of_fixed_assets + equity_to_asset_percentage +
                        debt_to_asset_percentage + business_size +
                        total_debt_to_total_equity_percentage + PE_ratio +
                        interest_cover_ratio + dividend_yield_ratio_percentage +
                        operating_revenue_per_employee_th +
                        costs_of_employees_operating_revenue_percentage + costs_per_unit_of_revenue + year_dummies[, -1],
                      data = data)


summary_baseline1 <- summary(baseline_model1)
summary(baseline_model2)
summary(baseline_model3)


#check sample size
source("https://slcladal.github.io/rscripts/SampleSizeMLR.r")
source("https://slcladal.github.io/rscripts/ExpR.r")
smplesz(baseline_model1)

write.csv(summary_baseline1$coefficients, file = "linear_regression_results.csv", row.names = TRUE)

#robustness & additional analysis
#check multicollinearity - Variance Inflation Factor (VIF) 
vif_values <- vif(baseline_model1)
print(vif_values)
write.csv(vif_values, file = "vif.csv", row.names = TRUE)


#Robustness Analysis
#check for heteroscedasticity
residuals <- residuals(baseline_model1)

plot_data <- data.frame(Fitted = fitted(baseline_model1), Residuals = residuals(baseline_model1))

ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Scatterplot of Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals")

#White test
white_test(baseline_model1)

#robust standard errors
robust_se <- vcovHC(baseline_model1, type = "HC3")
robse_baseline_model <- coeftest(baseline_model1, vcov = robust_se)

write.csv(robse_baseline_model, file = "rob_linear_regression_results.csv", row.names = TRUE)


#entity + time fixed effect model
fe_model <- plm(profit_margin_percentage ~ ratio_intangible_assets_total_assets + ratio_intangible_assets_on_total_operating_revenue +
                             ratio_of_fixed_assets + equity_to_asset_percentage +
                             debt_to_asset_percentage + business_size +
                             total_debt_to_total_equity_percentage + PE_ratio +
                             interest_cover_ratio + dividend_yield_ratio_percentage +
                             operating_revenue_per_employee_th +
                             costs_of_employees_operating_revenue_percentage + costs_per_unit_of_revenue + year_dummies,
                           data = data, 
                           model = "within", effect = "individual", index = "id")

fe_rob_se <- coeftest(fe_model, vcov. = vcovHC, type = "HC3")
print(fe_rob_se)

write.csv(fe_rob_se, file = "FE_results.csv", row.names = TRUE)