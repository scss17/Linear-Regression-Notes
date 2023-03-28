# Load libraries ----------------------------------------------------------
library(ggplot2)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(modelr)

# Load data set
original <- read_csv(file = "data/simulation.csv",
                     skip = 1,
                     show_col_types = FALSE,
                     col_names = c("answer_time",
                                   "personnel",
                                   "calls_per_hour",
                                   "time_per_call"))

# Nest the data 
model_data <- original  %>% 
        pivot_longer(cols = !personnel,
                     names_to = c("response")) %>%
        mutate(predictor = "personnel") %>% 
        group_by(response, predictor) %>% nest()

# Add summary statistics
model_data <- model_data %>% mutate(average = unlist(map(data, function(df) mean(df$value))),
                                sd = unlist(map(data, function(df) sd(df$value))))


# Create a function to fit linear regression model
response_model <- function(df) {
        lm(value ~ personnel, data = df)
}

# Add model to the nested data frame
model_data <- model_data %>% mutate(models = map(data, response_model))


# Simulation --------------------------------------------------------------

# Simulated vectors
set.seed(61424638)
personnel <- c(ceiling(runif(n = 120, min = 100, 120)), model_data$data[[1]][[1]])

answer_time <- c(rnorm(n = 120, mean = coefficients(model_data$models[[1]])[1] + 
                              coefficients(model_data$models[[1]])[2] * personnel, 
                      sd = model_data$sd[1]/2), model_data$data[[1]][[2]])
calls_per_hour <- c(rnorm(n = 120, mean = coefficients(model_data$models[[2]])[1] + 
                                  coefficients(model_data$models[[2]])[2] * personnel , 
                          sd = model_data$sd[2]), model_data$data[[2]][[2]])
time_per_call <- c(rnorm(n = 120, mean = coefficients(model_data$models[[3]])[1] + 
                                 coefficients(model_data$models[[3]])[2] * personnel, 
                         sd = model_data$sd[3]), model_data$data[[3]][[2]])


# Create simulated data frame
call_center <- data.frame(personnel, answer_time, calls_per_hour, time_per_call)


# Plots -------------------------------------------------------------------

# Original dataset
or_plot01 <- ggplot(mapping = aes(x = personnel), data = original)
or_plot01 + geom_point(aes(y = answer_time))
or_plot01 + geom_point(aes(y = calls_per_hour))
or_plot01 + geom_point(aes(y = time_per_call))

# Plot simulated dataset
s_plot01 <- ggplot(mapping = aes(x = personnel), data = call_center)
s_plot01 + geom_point(aes(y = answer_time))
s_plot01 + geom_point(aes(y = calls_per_hour))
s_plot01 + geom_point(aes(y = time_per_call))

# Create save simulated data set
write.csv(x = call_center, file = "data/call_center_regression")



