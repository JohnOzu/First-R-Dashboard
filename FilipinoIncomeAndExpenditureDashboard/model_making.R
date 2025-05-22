setwd('C:/Users/User/Documents/R/Dashboard_LE/FilipinoIncomeAndExpenditureDashboard')
data <- read_csv("cleaned_data.csv")

library(tidymodels)
library(tidyverse)

set.seed(18)
datasplit <- initial_split(data,
                           prop = 0.70,
                           strata = `Total Household Income`)

data_train <- training(datasplit)
data_train

data_test <- testing(datasplit)
data_test

ml_rec <- recipe(`Total Household Income` ~ ., data = data_train) |>
  step_normalize(all_numeric_predictors())
ml_rec

ml_model <- linear_reg() |> 
  set_mode("regression") |>
  set_engine("lm")
ml_model

ml_wf <- workflow() |>
  add_recipe(ml_rec) |>
  add_model(ml_model)
ml_wf

ml_trained <- ml_wf |>
  fit(data_train)
ml_trained

result_final <- ml_trained |>
  predict(data_test)
result_final