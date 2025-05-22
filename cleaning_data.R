library(tidyverse)

setwd('C:/Users/User/Documents/R/Dashboard_LE')
data <- read_csv('data.csv')

cleaned_data <- data |>
  distinct() |>
  drop_na() |>
  select(
    `Region`,
    `Total Household Income`,
    `Total Food Expenditure`,
    `Main Source of Income`,
    `Bread and Cereals Expenditure`,
    `Total Rice Expenditure`,
    `Meat Expenditure`,
    `Total Fish and  marine products Expenditure`,
    `Fruit Expenditure`,
    `Vegetables Expenditure`,
    `Restaurant and hotels Expenditure`,
    `Alcoholic Beverages Expenditure`,
    `Tobacco Expenditure`,
    `Clothing, Footwear and Other Wear Expenditure`,
    `Housing and water Expenditure`,
    `Imputed House Rental Value`,
    `Medical Care Expenditure`,
    `Transportation Expenditure`,
    `Communication Expenditure`,
    `Education Expenditure`,
    `Miscellaneous Goods and Services Expenditure`,
    `Special Occasions Expenditure`,
    `Crop Farming and Gardening expenses`,
    `Household Head Sex`,
    `Household Head Age`,
    `Household Head Highest Grade Completed`,
    `Type of Household`,
    `Total Number of Family members`,
    `Type of Building/House`,
  ) |>
  mutate(
    `Total Expenditure` = 
      `Bread and Cereals Expenditure` +
      `Total Rice Expenditure` +
      `Meat Expenditure` +
      `Total Fish and  marine products Expenditure` +
      `Fruit Expenditure` +
      `Vegetables Expenditure` +
      `Restaurant and hotels Expenditure` +
      `Alcoholic Beverages Expenditure` +
      `Tobacco Expenditure` +
      `Clothing, Footwear and Other Wear Expenditure` +
      `Housing and water Expenditure` +
      `Imputed House Rental Value` +
      `Medical Care Expenditure` +
      `Transportation Expenditure` +
      `Communication Expenditure` +
      `Education Expenditure` +
      `Miscellaneous Goods and Services Expenditure` +
      `Special Occasions Expenditure` +
      `Crop Farming and Gardening expenses`,
    `Income Per Capita` = `Total Household Income` / `Total Number of Family members`,
    `Net Savings Deficit` = `Total Household Income` - `Total Expenditure`,
    `Food Expenditure %` = round((`Total Food Expenditure` / `Total Household Income`) * 100)
  )

write_csv(cleaned_data, './FilipinoIncomeAndExpenditureDashboard/cleaned_data.csv')