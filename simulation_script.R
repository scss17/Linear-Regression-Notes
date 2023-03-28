# Load libraries ----------------------------------------------------------
library(ggplot2)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)

# Load data set
original <- read_csv(file = "data/simulation.csv",
                     skip = 1,
                     show_col_types = FALSE,
                     col_names = c("answer_time",
                                   "personnel",
                                   "calls_per_hour",
                                   "time_per_call"))


original <- original  %>% 
        pivot_longer(cols = !personnel,
                     names_to = c("response")) %>%
        mutate(predictor = "personnel") %>% 
        group_by(response, predictor) %>% nest()


original %>% mutate(model = map(data, ))

# Linear models
m1 <- lm(data = original, answer_time ~ personnel)
m2 <- lm(data = original, calls_per_hour ~ personnel)
m3 <- lm(data = original, time_per_call ~ personnel)

# Simulation --------------------------------------------------------------

# Simulated vectors
set.seed(61424638)
personnel <- c(ceiling(runif(n = 120, min = 100, 120)), original$personnel)
aswer_time <- c()
calls_per_hour <- c()
time_per_call <- c()

# Create simulated data frame
call_center <- data.frame(personnel, time_per_call, calls_per_hour)


# Plots -------------------------------------------------------------------

# Original dataset
or_plot01 <- ggplot(mapping = aes(x = personnel), data = original)
or_plot01 + geom_point(aes(y = answer_time))
or_plot01 + geom_point(aes(y = calls_per_hour))
or_plot01 + geom_point(aes(y = time_per_call))

# Plot simulated dataset
s_plot01 <- ggplot(mapping = aes(x = personnel), data = call_center)
s_plot01 + geom_point(aes(y = time_per_call))
s_plot01 + geom_point(aes(y = calls_per_hour))






