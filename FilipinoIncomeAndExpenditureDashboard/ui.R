library(shiny)
library(bslib)
library(highcharter)
library(plotly)
library(leaflet)
library(fontawesome)

page_navbar(
  title = "PIED",
  
  theme = bs_theme(
    version = 5,
    base_font = font_google("DM Sans"),
    primary = "#0d6efd",
    bg = "#f8f9fa",
    fg = "#212529"
  ),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css") # attach a css file for styling
  ),
  
  nav_panel( # fist nav panel
    title = "Dashboard",
    icon = icon("chart-line"),
    
    layout_sidebar( # dashboard nav panel side bar for filters
      sidebar = sidebar(
        title = "Filters",
        width = 330,
        
        selectInput("region", "Major Island", 
                    choices = c("All", "Luzon", "Visayas", "Mindanao"),
                    selected = "All"),
        selectInput("family_type", "Type of Family:", 
                    choices = c("All", "Extended Family", "Single Family", "Two or More Nonrelated Persons/Members"),
                    selected = "All"),
        selectInput("head_sex", "Household Head Sex:", 
                    choices = c("All", "Male", "Female"),
                    selected = "All"),
        selectInput("building_type", "Type of Building/House:", 
                    choices = c("All", "Single house", "Duplex", "Commercial/industrial/agricultural building", "Multi-unit residential", "Institutional living quarter", "Other building unit (e.g. cave, boat)"),
                    selected = "All"),
        selectInput("head_grade", "Household Head Grade Completed:", 
                    choices = c("All", "No Education", "Elementary", "High School", "College", "Vocational"),
                    selected = "All"),
        
        actionButton("apply_filters", "Apply Filters", 
                     class = "btn-primary w-100"),
        actionButton("reset_filters", "Reset Filters", 
                     class = "btn-primary w-100")
      ),
      
      # Main Header for dashboard nav panel
      div(
        div(class = "page-header mb-4", 
            h2("Philippine Income & Expenditure Dashboard", class = "header-title"),
            p("Interactive visualizations of household income and expenditure data", class = 'text-center')
        ),
        
        # 3 cards below main header
        div(
          layout_columns(
            col_widths = 12,
            uiOutput("avg_household_income")
          )
        ),
        
        # map of the Philippines
        layout_columns(
          col_widths = c(5, 7),
          
          card(
            card_header("Regional Map of Philippines"),
            leafletOutput("map", height = "400px")
          ),
          
          # nav card tab for visualizations related to income
          navset_card_tab(
            full_screen = TRUE,
            nav_panel("Total Household Income", highchartOutput("incomeOne_chart", height = "400px")),
            nav_panel("Average Household Income", highchartOutput("incomeTwo_chart", height = "400px")),
            nav_panel("Income Per Capita", highchartOutput("incomeThree_chart", height = "400px"))
          )
        ),
        
        layout_columns(
          col_widths = c(8,4),
          
          # nav card tab for visualizations related to expenditure
          navset_card_tab(
            full_screen = TRUE,
            nav_panel("Total Household Expenditure", highchartOutput("expenditureOne_chart", height = "400px")),
            nav_panel("Household Expenditure Distribution", highchartOutput("expenditureTwo_chart", height = "400px")),
            nav_panel("Average Expenditure & Family Members", highchartOutput("expenditureThree_chart", height = "400px"))
          ),
          
          # card related to visualization of income source
          card(
            card_header("Income Source"),
            highchartOutput("source_chart", height = "400px")
          )
        )
      )
    )
  ),
  
  nav_panel( # second nav panel which holds the table of the dataset
    title = "Dataset",
    icon = icon("table"),
    
    div(class = "page-header", 
        h2("Income & Expenditure Dataset Explorer", class = "header-title"),
        p("Browse & filter the dataset", class = 'text-center')
    ),
    card(
      card_header("Dataset"),
      div(
        class = "p-3",
        DT::dataTableOutput("data_table")
      )
    )
  ),
  
  nav_panel( # third panel that holds the information about this dashboard
    title = "About",
    icon = icon("info-circle"),
    
    div(class = "page-header mb-4", 
        h2("About the PIED Project", class = "header-title"),
        p("Information and resources about the Philippine Income & Expenditure Dashboard", class = 'text-center')
    ),
    card(
      card_header("About PIED Dashboard"),
      div(
        class = "px-5 pt-2",
        h3("Philippine Income & Expenditure Dashboard", class = "fw-bold"),
        p("This dashboard provides visualization and analysis of household income and expenditure data in the Philippines."),
        
        h4("Dashboard Panel", class = "fw-semibold"),
        h6("Sidebar Filters: ", class = "fw-semibold"),
        tags$ul(
          tags$li(HTML("<strong>Major Island:</strong> All, Luzon, Visayas Mindanao")),
          tags$li(HTML("<strong>Type of Family:</strong> All, Extended Family, Two or More Nonrelated Persons/Members")),
          tags$li(HTML("<strong>Household Head Sex:</strong> All, Male, Female")),
          tags$li(HTML("<strong>Type of Building/House:</strong> Single House, Duplex, Commercial/Industrial/Agriculture Building")),
          tags$li(HTML("<strong>Household Head Grade Completed:</strong> No Education, Elementary, High School, College, Vocational")),
        ),
        h6("Main Content: ", class = "fw-semibold"),
        tags$ul(
          tags$li(
            HTML("<strong>Average Household Income Card </strong><br>
                 This card displays the average household income in the dataset.")
          ),
          tags$li(
            HTML("<strong>Avergae Houseld Head Age Card </strong><br>
                 This card displays the average household head age that is present in the dataset.")
          ),
          tags$li(
            HTML("<strong>Average Number of Family Members Card </strong><br>
                 This card displays the average number of family numbers in the dataset.")
          ),
          tags$li(
            HTML("<strong>Regional Map of the Philippines </strong><br>
                 This displays the map of the Philippines and the borders of each regions present in the Philippines.<br>
                 Also, you can hover each regions to display its information about: <br>
                 &emsp; - Average Household Income <br>
                 &emsp; - Average Number of Family Members <br>
                 &emsp; - Average Food Expenditure <br>
                 &emsp; - Main Source of Income <br>
                 This highlights regional differences in income levels, average family size, food expenditure, and main sources of income.")
          ),
          tags$li(
            HTML("<strong>Income Card Tab: </strong><br>
                 This card holds 3 charts which displays infomation about: <br>
                 &emsp; - Total Household Income (Bar Chart) <br>
                 &emsp; - Average Household Income (Row Chart) <br>
                 &emsp; - Income Per Capita (Radial Bar Chart)<br>
                 This shows the total, average, and per capita income generated by each region.")
          ),
          tags$li(
            HTML("<strong>Expenditure Card Tab: </strong><br>
                 This card also holds 3 charts which displays information about: <br>
                 &emsp; - Total Household Income Expenditure (Bar Chart) <br>
                 &emsp; - Household Expenditure Distribution (Tree Map) <br>
                 &emsp; - Average Expenditure & Number of Family Members (Grouped Bar Chart)<br>
                 This shows the total, distribution, and average expenditure of each region, along with the average number of family members.")
          ),
          tags$li(
            HTML("<strong>Income Source Card: </strong><br>
                 This displays the distribution of income sources for Filipinos, providing insights into the various channels through which they earn.")
          ),
        ),
        h4("Dataset Panel", class = "fw-semibold"),
        p("This displays the dataset used to create the visualizations in this dashboard. Here you can freely explore the dataset and filter columns"),
        
        h4("About Panel", class = "fw-semibold"),
        p("This page provides information about the structure and layout of the dashboard."),
        
        h4("Data Source", class = "fw-semibold"),
        a("https://www.kaggle.com/datasets/grosvenpaul/family-income-and-expenditure", href = "https://www.kaggle.com/datasets/grosvenpaul/family-income-and-expenditure"),
        HTML("<br>"),
        a("https://psa.gov.ph/statistics/income-expenditure/fies", href = "https://psa.gov.ph/statistics/income-expenditure/fies"),
        HTML("<br><br>"),
        
        h4("About the Creator", class = "fw-semibold"),
        p(HTML(
          "<strong> John Lester M. Capote </strong> <br>
           Bachelor of Science in Computer Science Major in Data Science <br>
           University of Southeastern Philippines" 
        ))
      )
    )
  )
)
