library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(fontawesome)
library(scales)
library(highcharter)

ph_regions <- st_read("geoBoundaries-PHL-ADM1_simplified.geojson")
data <- read_csv("cleaned_data.csv")

function(input, output, session) {
  
  # this is for the filters in the dashboard
  filtered_data <- eventReactive(input$apply_filters, {
    
    df <- data
    
    if(input$region != "All") {
      
      if(input$region == "Luzon") {
        luzon_regions <- c("Ilocos Region", "Cagayan Valley", "CAR", "Central Luzon", "NCR", "Calabarzon", "Mimaropa", "Bicol Region", "ARMM")
        df <- df |> filter(Region %in% luzon_regions)
      } else if(input$region == "Visayas") {
        visayas_regions <- c("Western Visayas", "Central Visayas", "Eastern Visayas")
        df <- df |> filter(Region %in% visayas_regions)
      } else if(input$region == "Mindanao") {
        mindanao_regions <- c("Zamboanga Peninsula", "Northern Mindanao", "Davao Region", "Soccsksargen", "Caraga", "ARMM")
        df <- df |> filter(Region %in% mindanao_regions)
      }
    }
    
    if(input$family_type != "All") {
      df <- df |> filter(`Type of Household` == input$family_type)
    }
    
    if(input$head_sex != "All") {
      df <- df |> filter(`Household Head Sex` == input$head_sex)
    }
    
    if(input$building_type != "All") {
      df <- df |> filter(`Type of Building/House` == input$building_type)
    }
    
    if(input$head_grade != "All") {
      
      if(input$head_grade == "No Education") {
        no_educ <- c("No Grade Completed", "Preschool")
        df <- df |> filter(`Household Head Highest Grade Completed` %in% no_educ)
      } else if(input$head_grade == "Elementary") {
        elementary <- c("Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", "Grade 6", "Elementary Graduate")
        df <- df |> filter(`Household Head Highest Grade Completed` %in% elementary)
      } else if(input$head_grade == "High School") {
        high_school <- c("Second Year High School", 
                         "Third Year High School", 
                         "First Year High School", 
                         "Fourth Year High School", 
                         "High School Graduate", 
                         "Other Programs Not Equal to a Degree")
        df <- df |> filter(`Household Head Highest Grade Completed` %in% high_school)
      } else if(input$head_grade == "College") {
        college <- c("Teacher Training and Education Sciences Programs",
                     "Business and Administration Programs",
                     "First Year College",
                     "Humanities Programs",
                     "Engineering and Engineering Trades Programs",
                     "Social and Behavioral Sciences Programs",
                     "Agriculture, Forestry, and Fisher Programs",
                     "Health Programs",
                     "Fourth Year College",
                     "Second Year College",
                     "Third Year College",
                     "Security Services Programs",
                     "First Year Post Secondary",
                     "Second Year Post Secondary",
                     "Post Baccalaureate",
                     "Computing/Information Technology Programs",
                     "Mathematics and Statistics Programs",
                     "Personal Services Programs",
                     "Law Programs",
                     "Journalism and Information Programs",
                     "Manufacturing and Processing Programs",
                     "Life Sciences Programs",
                     "Other Programs Equal to a Degree",
                     "Social Services Programs",
                     "Physical Sciences Programs",
                     "Arts Programs",
                     "Veterninary Programs",
                     "Environmental Protection Programs")
        df <- df |> filter(`Household Head Highest Grade Completed` %in% college)
      } else if(input$head_grade == "Vocational") {
        vocational <- c("Transport Services Programs",
                        "Agriculture, Forestry, and Fishery Programs",
                        "Basic Programs")
        df <- df |> filter(`Household Head Highest Grade Completed` %in% vocational)
      }
    }
    
    return(df)
  }, ignoreNULL = FALSE)
  
  # this is for reseting filters
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "region", selected = "All")
    updateSelectInput(session, "family_type", selected = "All")
    updateSelectInput(session, "head_sex", selected = "All")
    updateSelectInput(session, "building_type", selected = "All")
    updateSelectInput(session, "head_grade", selected = "All")

  })

  # output for the 3 cards below the main header
  output$avg_household_income <- renderUI({
    
    current_data <- filtered_data()
    req(nrow(current_data) > 0)
    
    avg_income <- mean(current_data$`Total Household Income`, na.rm = TRUE)
    avg_household_age <- mean(current_data$`Household Head Age`, na.rm = TRUE)
    avg_household_member <- mean(current_data$`Total Number of Family members`, na.rm = TRUE)
    
    tagList(
      div(
        class = "upper-container",
        
        # Income Box
        div(
          class = "info-box",
          div(icon("money-bill-wave", class = "fa-2x"), style = "margin-right: 25px;"),
          div(
            p("Avg. Household Income", class = "title"),
            p(paste0("₱", format(avg_income, big.mark = ",", scientific = FALSE)), class = "value")
          )
        ),
        
        # Age Box
        div(
          class = "info-box",
          div(icon("person-cane", class = "fa-2x"), style = "margin-right: 25px;"),
          div(
            p("Avg. Household Head Age", class = "title"),
            p(round(avg_household_age, 0), class = "value")
          )
        ),
        
        # Family Members Box
        div(
          class = "info-box",
          div(icon("users", class = "fa-2x"), style = "margin-right: 25px;"),
          div(
            p("Avg. Number of Family Members", class = "title"),
            p(round(avg_household_member, 0), class = "value")
          )
        )
      )
    )
  })
  
  # output for the Philippine Map
  output$map <- renderLeaflet({
    
    map_pop_up <- data |>
      group_by(Region) |>
      summarise(avg_income = mean(`Total Household Income`, na.rm = TRUE),
                avg_member = mean(`Total Number of Family members`, na.rm = TRUE),
                avg_food = mean(`Total Food Expenditure`, na.rm = TRUE),
                main_income = {
                  tbl <- table(`Main Source of Income`)
                  if (length(tbl) > 0) names(tbl)[which.max(tbl)] else NA_character_
                },
                .groups = 'drop')
    
    ph_regions_for_map <- ph_regions |>
      left_join(map_pop_up, by = c("shapeName" = "Region"))
    
    leaflet_map <- leaflet(ph_regions_for_map) |>
      addProviderTiles("Esri.WorldImagery")
    
    for(i in 1:nrow(ph_regions_for_map)) {
      region_data <- ph_regions_for_map[i,]
      
      region_label <- htmltools::HTML(paste0(
        "<strong>Region: </strong>", region_data$shapeName, "<br>",
        "<strong>Avg. Household Income: </strong>₱",
        format(round(region_data$avg_income, 0), nsmall = 0, big.mark = ",", scientific = FALSE), "<br>",
        "<strong>Avg. Number of Family Members: </strong>",
        round(region_data$avg_member, 0), "<br>",
        "<strong>Avg. Food Expenditures: </strong>₱",
        format(round(region_data$avg_food, 0), nsmall = 0, big.mark = ",", scientific = FALSE), "<br>",
        "<strong>Main Source of Income: </strong>",
        region_data$main_income
      ))
      
      # Add this polygon to the map
      leaflet_map <- leaflet_map |>
        addPolygons(
          data = region_data,
          fillColor = "#6F9CEB",
          color = "white",
          weight = 1,
          layerId = region_data$shapeName,
          label = region_label,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px", 
                         "font-size" = "13px", "color" = "black",
                         "background-color" = "rgba(255, 255, 255, 0.85)"),
            direction = "auto",
            sticky = FALSE, 
            noHide = FALSE 
          ),
          highlightOptions = highlightOptions(
            weight = 3,
            color = "red",
            bringToFront = TRUE
          )
        )
    }
    
    return(leaflet_map) 
  })
  
  # first chart related to income
  
  output$incomeOne_chart <- renderHighchart({
    
    current_data <- filtered_data()
    req(nrow(current_data) > 0)
    
    household_income_region <- current_data |>
      group_by(Region) |>
      summarise(household_income_region = sum(`Total Household Income`, na.rm = TRUE)) |>
      arrange(desc(household_income_region))
    
    if (nrow(household_income_region) == 0) {
      return(highchart() |> hc_title(text = "Total Household Income") |> hc_subtitle(text = "No data for selected filters"))
    }
    
    highchart() |>
      hc_chart(type = "column") |>
      hc_title(text = "Total Household Income") |>
      hc_xAxis(
        categories = household_income_region$Region,
        title = list(text = "Region"),
        labels = list(rotation = -45)
      ) |>
      hc_yAxis(
        title = list(text = "Income"),
        labels = list(format = "{value:,.0f}")
      ) |>
      hc_series(list(
        name = "Income",
        data = round(household_income_region$household_income_region),
        colorByPoint = TRUE
      )) |>
      hc_colors(colorRampPalette(c("#FF5A5F", "#FFB3B3"))(nrow(household_income_region))) |>
      hc_plotOptions(column = list(
        pointWidth = 30,
        borderRadius = 2
      )) |>
      hc_tooltip(pointFormat = "Income: <b>{point.y:,.0f}</b>")
  })
  
  # second chart related to income
    
  output$incomeTwo_chart <- renderHighchart({
    
    current_data <- filtered_data()
    req(nrow(current_data) > 0)
    
    avg_income_region <- current_data |>
      group_by(Region) |>
      summarise(avg_income = mean(`Total Household Income`, na.rm = TRUE)) |>
      arrange(desc(avg_income)) 
    
    if (nrow(avg_income_region) == 0) {
      return(highchart() |> hc_title(text = "Average Household Income") |> hc_subtitle(text = "No data for selected filters"))
    }
    
    highchart() |>
      hc_chart(type = "column", inverted = TRUE) |> 
      hc_title(text = "Average Household Income") |>
      
      hc_xAxis(
        categories = avg_income_region$Region, 
        title = list(text = "Region")
      ) |>
      
      hc_yAxis(
        title = list(text = "Avg. Income"),
        labels = list(format = "{value:,.0f}", rotation = -45)
      ) |>
      
      hc_series(list(
        name = "Avg. Income",
        data = round(avg_income_region$avg_income),
        colorByPoint = TRUE
      )) |>
      
      hc_colors(colorRampPalette(c("#FF5A5F", "#FFB3B3"))(nrow(avg_income_region))) |>
      
      hc_plotOptions(column = list(              
        borderRadius = 2,
        pointWidth = 12
      )) |>
      
      hc_tooltip(pointFormat = "Avg. Income: <b>{point.y:,.0f}</b>")
  })
  
  # third chart related to income
  
  output$incomeThree_chart <- renderHighchart({
    
    current_data <- filtered_data()
    req(nrow(current_data) > 0)
    
    income_capita_region <- current_data |>
      group_by(Region) |>
      summarise(income_capita = sum(`Total Household Income`, na.rm = TRUE) / sum(`Total Number of Family members`, na.rm = TRUE)) |>
      arrange(desc(income_capita))
    
    if (nrow(income_capita_region) == 0 || all(is.na(income_capita_region$income_capita))) {
      return(highchart() |> hc_title(text = "Income Per Capita") |> hc_subtitle(text = "No data for selected filters"))
    }
    
    gradient_color <- list(
      radialGradient = list(cx = 0.5, cy = 0.5, r = 0.7),
      stops = list(
        list(0, "rgba(255, 90, 95, 0.9)"),
        list(1, "rgba(255, 90, 95, 0.3)")
      )
    )
    
    highchart() |>
      hc_chart(polar = TRUE, type = "column") |>
      hc_title(text = NULL) |>
      hc_xAxis(
        categories = income_capita_region$Region,
        tickmarkPlacement = "on",
        lineWidth = 0
      ) |>
      hc_yAxis(
        gridLineInterpolation = "polygon",
        lineWidth = 0,
        min = 0,
        max = max(income_capita_region$income_capita) * 1.1
      ) |>
      hc_series(list(
        name = "Income per Capita",
        data = round(income_capita_region$income_capita, 2),
        pointPlacement = "on",
        color = gradient_color
      )) |>
      hc_plotOptions(
        column = list(
          pointPadding = 0,
          groupPadding = 0,
          borderWidth = 0
        )
      ) |>
      hc_legend(enabled = FALSE)
  })
  
  # first chart related to expenditure
  
  output$expenditureOne_chart <- renderHighchart({
    
    current_data <- filtered_data()
    req(nrow(current_data) > 0)
    
    total_expenditure = current_data |>
      group_by(Region) |>
      summarise(total = sum(`Total Expenditure`)) |>
      arrange(desc(total))
    
    if (nrow(total_expenditure) == 0) {
      return(highchart() |> hc_title(text = "Total Household Expenditure by Region") |> hc_subtitle(text = "No data for selected filters"))
    }
    
    highchart() |>
      hc_chart(type = "column") |> 
      hc_title(text = "Total Household Expenditure by Region") |>
      
      hc_xAxis(
        categories = total_expenditure$Region, 
        title = list(text = "Region")
      ) |>
      
      hc_yAxis(
        title = list(text = "Total Expenditure"),
        labels = list(format = "{value:,.0f}", rotation = -45)
      ) |>
      
      hc_series(list(
        name = "Total Expenditure",
        data = round(total_expenditure$total),
        colorByPoint = TRUE
      )) |>
      
      hc_colors(colorRampPalette(c("#1F9CEB", "#6F9CEB"))(nrow(total_expenditure))) |>
      
      hc_plotOptions(column = list(              
        pointWidth = 35,
        borderRadius = 2
      )) |>
      
      hc_tooltip(pointFormat = "Total: <b>{point.y:,.0f}</b>")
  })
  
  # second chart related to expenditure
  
  output$expenditureTwo_chart <- renderHighchart({
    
    current_data <- filtered_data()
    req(nrow(current_data) > 0)
    
    expenditure_type <- current_data |>
      summarise(across(c(
        `Total Food Expenditure`,
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
        `Crop Farming and Gardening expenses`
      ), sum, na.rm = TRUE))
    
    expenditure_long <- expenditure_type |>
      pivot_longer(cols = everything(), names_to = "category", values_to = "value") |>
      filter(value > 0)
    
    if (nrow(expenditure_long) == 0) {
      return(highchart() |> hc_title(text = "Household Expenditure Treemap") |> hc_subtitle(text = "No expenditure data for selected filters"))
    }
    
    highchart() |>
      hc_chart(type = "treemap") |>
      hc_title(text = "Household Expenditure Treemap") |>
      hc_plotOptions(
        treemap = list(
          layoutAlgorithm = "squarified",
          allowDrillToNode = TRUE,
          dataLabels = list(
            enabled = TRUE,
            format = '{point.name}'
          )
        )
      ) |>
      hc_series(
        list(
          type = "treemap",
          layoutAlgorithm = "squarified",
          allowDrillToNode = TRUE,
          levelIsConstant = FALSE,
          levels = list(
            list(level = 1, dataLabels = list(enabled = TRUE))
          ),
          data = lapply(1:nrow(expenditure_long), function(i) {
            list(
              name = expenditure_long$category[i],
              value = expenditure_long$value[i],
              colorValue = expenditure_long$value[i]  # Used for palette coloring
            )
          })
        )
      ) |>
      hc_colorAxis(
        stops = list(
          list(0.0, "#E8F7EE"),
          list(0.5, "#FF5A5F"),
          list(1.0, "#6F9CEB")
        )
      )
  })
  
  # third chart related to expenditure
  
  output$expenditureThree_chart <- renderHighchart({
    
    current_data <- filtered_data()
    req(nrow(current_data) > 0)
    
    family_expenditure <- current_data |>
      group_by(Region) |>
      summarise(avg_expenditure = mean(`Total Expenditure`, na.rm = TRUE),
                avg_member = mean(`Total Number of Family members`, na.rm = TRUE)) |>
      arrange(desc(avg_member))
    
    highchart() |>
      hc_chart(type = "bar") |>
      hc_title(text = "Average Expenditure and Family Size by Region") |>
      hc_xAxis(
        categories = family_expenditure$Region,
        title = list(text = "Region")
      ) |>
      hc_yAxis_multiples(
        list(
          title = list(text = "Average Expenditure"),
          opposite = FALSE
        ),
        list(
          title = list(text = "Average Family Size"),
          opposite = TRUE
        )
      ) |>
      hc_plotOptions(bar = list(grouping = TRUE)) |>
      hc_series(
        list(
          name = "Avg Expenditure",
          data = round(family_expenditure$avg_expenditure, 2),
          yAxis = 0
        ),
        list(
          name = "Avg Family Size",
          data = round(family_expenditure$avg_member, 1),
          yAxis = 1
        )
      ) |>
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  
  # chart for main source of income distribution
  
  output$source_chart <- renderHighchart({
    
    current_data <- filtered_data()
    req(nrow(current_data) > 0)
    
    income_source_region <- current_data |>
      group_by(`Main Source of Income`) |>
      summarise(count = n()) |>
      filter(count > 0)
    
    if (nrow(income_source_region) == 0) {
      return(highchart() |> hc_title(text = "Source of Income") |> hc_subtitle(text = "No data for selected filters"))
    }
    
     highchart() |>
      hc_chart(type = "pie") |>
      hc_title(text = "Source of Income") |>
      hc_plotOptions(
        pie = list(
          innerSize = '40%',
          dataLabels = list(enabled = TRUE, format = '<b>{point.name}</b>: {point.y}'),
          showInLegend = TRUE
        )
      ) |>
      hc_series(
        list(
          name = "Count",
          colorByPoint = TRUE,
          data = list_parse2(income_source_region)
        )
      ) |>
      hc_colors(c("#E8F7EE", "#FF5A5F", "#6F9CEB"))
  })
  
  # output for the display of the dataset
  
  output$data_table <- renderDataTable({
    data
  })
}
