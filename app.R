library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(RColorBrewer)

tempdir <- tempdir()
dir.create(tempdir, showWarnings = FALSE)
options(shiny.tempdir = tempdir)

# Function to generate summary for a given tech tool column
tech_summary <- function(data, col_name) {
  data %>%
    select(respondent_id, {{col_name}}) %>%
    separate_rows({{col_name}}, sep = ";\\s*") %>% 
    filter({{col_name}} != "None", {{col_name}} != "") %>%  
    group_by(tool = {{col_name}}) %>%  
    summarise(
      count = n(),
      respondent_ids = paste(unique(respondent_id), collapse = ";")
    ) %>%
    arrange(desc(count)) %>%
    ungroup()
}

cleaned_data <- read_csv("cleaned_data.csv", show_col_types = FALSE)

# Define UI for the dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Eduvos Graduate Tech Trends"),
  dashboardSidebar(
    selectInput("campus", "Campus", choices = unique(cleaned_data$campus), multiple = TRUE),
    selectInput("studyfield", "Field of Study", choices = unique(cleaned_data$studyfield), multiple = TRUE)
  ),
  dashboardBody(
    tabsetPanel(
      # General Overview Tab
      tabPanel("General Overview",
        fluidRow(
          box(plotlyOutput("campus_dist", height = 300), width = 6),
          box(plotlyOutput("field_dist", height = 300), width = 6)
        ),
        fluidRow(
          box(textOutput("survey_count"), width = 4),
          box(plotlyOutput("top_campuses", height = 300), width = 8)
        )
      ),
      # Employment Trends Tab
      tabPanel("Employment Trends",
        fluidRow(
          box(plotlyOutput("employment_status", height = 400), width = 12)
        ),
        fluidRow(
          box(plotlyOutput("industry_dist", height = 400), width = 12)
        ),
        fluidRow(
          box(plotlyOutput("role_dist", height = 600), width = 12)
        )
      ),
      # Tech Tools Tab
      tabPanel("Tech Tools",
        fluidRow(
          column(6,
            h3("Programming Languages"),
            selectInput("proglang_filter", "Select Programming Language", 
                        choices = NULL, multiple = TRUE),
            tabsetPanel(
              tabPanel("Bar Chart", plotlyOutput("proglang_bar", height = 400)),
              tabPanel("Dot Plot", plotlyOutput("proglang_dot", height = 400)),
              tabPanel("Pie Chart", plotlyOutput("proglang_pie", height = 400))
            )
          ),
          column(6,
            h3("Databases"),
            selectInput("database_filter", "Select Database", 
                        choices = NULL, multiple = TRUE),
            tabsetPanel(
              tabPanel("Bar Chart", plotlyOutput("database_bar", height = 400)),
              tabPanel("Dot Plot", plotlyOutput("database_dot", height = 400)),
              tabPanel("Pie Chart", plotlyOutput("database_pie", height = 400))
            )
          )
        ),
        fluidRow(
          column(6,
            h3("Web Frameworks"),
            selectInput("webframework_filter", "Select Web Framework", 
                        choices = NULL, multiple = TRUE),
            tabsetPanel(
              tabPanel("Bar Chart", plotlyOutput("framework_bar", height = 400)),
              tabPanel("Dot Plot", plotlyOutput("framework_dot", height = 400)),
              tabPanel("Pie Chart", plotlyOutput("framework_pie", height = 400))
            )
          ),
          column(6,
            h3("AI Developer Tools"),
            selectInput("aitool_filter", "Select AI Developer Tool", 
                        choices = NULL, multiple = TRUE),
            tabsetPanel(
              tabPanel("Bar Chart", plotlyOutput("ai_bar", height = 400)),
              tabPanel("Dot Plot", plotlyOutput("ai_dot", height = 400)),
              tabPanel("Pie Chart", plotlyOutput("ai_pie", height = 400))
            )
          )
        )
      ),
      # Raw Data Tab
      tabPanel("Raw Data", DTOutput("raw_table"))
    )
  )
)

# Define server logic for the dashboard
server <- function(input, output, session) {
  # Reactive filtered data based on Campus and Field of Study
  filtered_data <- reactive({
    campus_selected <- if (!is.null(input$campus)) input$campus else unique(cleaned_data$campus)
    studyfield_selected <- if (!is.null(input$studyfield)) input$studyfield else unique(cleaned_data$studyfield)
    cleaned_data %>% 
      filter(campus %in% campus_selected, studyfield %in% studyfield_selected)
  })
  
  filtered_data_debounced <- debounce(filtered_data, 500)
  
  proglang_filtered <- reactive({ tech_summary(filtered_data_debounced(), proglang) })
  databases_filtered <- reactive({ tech_summary(filtered_data_debounced(), databases) })
  webframework_filtered <- reactive({ tech_summary(filtered_data_debounced(), webframework) })
  aitool_filtered <- reactive({ tech_summary(filtered_data_debounced(), aitool) })
  
  observe({
    updateSelectInput(session, "proglang_filter", choices = unique(proglang_filtered()$tool))
    updateSelectInput(session, "database_filter", choices = unique(databases_filtered()$tool))
    updateSelectInput(session, "webframework_filter", choices = unique(webframework_filtered()$tool))
    updateSelectInput(session, "aitool_filter", choices = unique(aitool_filtered()$tool))
  })
 
  campus_agg <- reactive({
    filtered_data_debounced() %>%
      distinct(respondent_id, campus) %>%
      count(campus)
  })
  field_agg <- reactive({
    filtered_data_debounced() %>%
      distinct(respondent_id, studyfield) %>%
      count(studyfield) %>%
      arrange(desc(n)) %>%
      mutate(studyfield = factor(studyfield, levels = studyfield))
  })
  employment_agg <- reactive({ filtered_data_debounced() %>% count(employment) })
  industry_agg <- reactive({ filtered_data_debounced() %>% distinct(respondent_id, industry) %>% count(industry) })
  role_agg <- reactive({ filtered_data_debounced() %>% distinct(respondent_id, role) %>% count(role) })
  output$survey_count <- renderText({
    paste("Total Respondents:", nrow(filtered_data_debounced()))
  })
  output$campus_dist <- renderPlotly({
    p <- campus_agg() %>%
      ggplot(aes(x = reorder(campus, n), y = n, fill = campus)) +
      geom_col(show.legend = FALSE) +
      labs(title = "Campus Distribution", x = "Campus", y = "Respondents") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  output$field_dist <- renderPlotly({
    data <- field_agg()
    plot_ly(data, 
            labels = ~studyfield, 
            values = ~n, 
            type = 'pie',
            name = 'Field of Study',
            marker = list(colors = brewer.pal(n_distinct(data$studyfield), "Set3"))) %>%
      layout(
        title = 'Field of Study Distribution',
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0.5, y = -0.1, xanchor = "center"),
        margin = list(l = 50, r = 50, b = 100, t = 100, pad = 4)
      )
  })
  output$top_campuses <- renderPlotly({
    top_data <- campus_agg() %>% arrange(desc(n)) %>% head(3)
    p <- ggplot(top_data, aes(x = reorder(campus, n), y = n, fill = campus)) +
      geom_col(show.legend = FALSE) +
      labs(title = "Top 3 Campuses", x = "Campus", y = "Respondents") +
      theme_minimal()
    ggplotly(p)
  })
  output$employment_status <- renderPlotly({
    p <- employment_agg() %>%
      ggplot(aes(x = reorder(employment, n), y = n, fill = employment)) +
      geom_col(show.legend = FALSE) +
      labs(title = "Employment Statuses", x = "Status", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      coord_flip()
    ggplotly(p)
  })
  output$industry_dist <- renderPlotly({
    p <- industry_agg() %>%
      ggplot(aes(x = reorder(industry, n), y = n, fill = industry)) +
      geom_col(show.legend = FALSE) +
      labs(title = "Industry Distribution", x = "Industry", y = "Respondents") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      coord_flip()
    ggplotly(p)
  })
  output$role_dist <- renderPlotly({
    p <- role_agg() %>%
      ggplot(aes(x = reorder(role, n), y = n, fill = role)) +
      geom_col(show.legend = FALSE) +
      labs(title = "Developer Roles", x = "Role", y = "Respondents") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      coord_flip()
    ggplotly(p)
  })
  # -------------------------
  # Programming Languages Visualizations
  # -------------------------
  output$proglang_bar <- renderPlotly({
    filtered <- proglang_filtered() %>% filter(tool %in% input$proglang_filter)
    if (nrow(filtered) == 0) {
      p <- ggplot() + labs(title = "Programming Languages", x = "Language", y = "Usage Count") + theme_minimal()
      return(ggplotly(p))
    }
    p <- ggplot(filtered, aes(x = reorder(tool, count), y = count, fill = tool)) +
      geom_col(show.legend = FALSE) +
      labs(title = "Programming Languages", x = "Language", y = "Usage Count") +
      theme_minimal() +
      coord_flip()
    ggplotly(p)
  })
  output$proglang_dot <- renderPlotly({
    filtered <- proglang_filtered() %>% filter(tool %in% input$proglang_filter)
    if(nrow(filtered) == 0) {
      p <- ggplot() + labs(title = "Programming Languages Dot Plot", x = "Usage Count", y = "Language") + theme_minimal()
      return(ggplotly(p))
    }
    p <- ggplot(filtered, aes(x = count, y = reorder(tool, count))) +
      geom_point(size = 3, color = "steelblue") +
      labs(title = "Programming Languages Dot Plot", x = "Usage Count", y = "Language") +
      theme_minimal()
    ggplotly(p)
  })
  output$proglang_pie <- renderPlotly({
    filtered <- proglang_filtered() %>% filter(tool %in% input$proglang_filter)
    if(nrow(filtered) == 0) {
      return(plotly_empty(type = "pie", title = "Programming Languages Pie Chart"))
    }
    plot_ly(filtered, labels = ~tool, values = ~count, type = 'pie',
            textinfo = 'label+percent', insidetextorientation = 'radial') %>%
      layout(title = "Programming Languages Pie Chart")
  })
  # -------------------------
  # Databases Visualizations
  # -------------------------
  output$database_bar <- renderPlotly({
    filtered <- databases_filtered() %>% filter(tool %in% input$database_filter)
    if(nrow(filtered) == 0) {
      p <- ggplot() + labs(title = "Databases", x = "Database", y = "Usage Count") + theme_minimal()
      return(ggplotly(p))
    }
    p <- ggplot(filtered, aes(x = reorder(tool, count), y = count, fill = tool)) +
      geom_col(show.legend = FALSE) +
      labs(title = "Databases", x = "Database", y = "Usage Count") +
      theme_minimal() +
      coord_flip()
    ggplotly(p)
  })
  output$database_dot <- renderPlotly({
    filtered <- databases_filtered() %>% filter(tool %in% input$database_filter)
    if(nrow(filtered) == 0) {
      p <- ggplot() + labs(title = "Databases Dot Plot", x = "Usage Count", y = "Database") + theme_minimal()
      return(ggplotly(p))
    }
    p <- ggplot(filtered, aes(x = count, y = reorder(tool, count))) +
      geom_point(size = 3, color = "darkorange") +
      labs(title = "Databases Dot Plot", x = "Usage Count", y = "Database") +
      theme_minimal()
    ggplotly(p)
  })
  output$database_pie <- renderPlotly({
    filtered <- databases_filtered() %>% filter(tool %in% input$database_filter)
    if(nrow(filtered) == 0) {
      return(plotly_empty(type = "pie", title = "Databases Pie Chart"))
    }
    plot_ly(filtered, labels = ~tool, values = ~count, type = 'pie',
            textinfo = 'label+percent', insidetextorientation = 'radial') %>%
      layout(title = "Databases Pie Chart")
  })
  # -------------------------
  # Web Frameworks Visualizations
  # -------------------------
  output$framework_bar <- renderPlotly({
    filtered <- webframework_filtered() %>% filter(tool %in% input$webframework_filter)
    if(nrow(filtered) == 0) {
      p <- ggplot() + labs(title = "Web Frameworks", x = "Framework", y = "Usage Count") + theme_minimal()
      return(ggplotly(p))
    }
    p <- ggplot(filtered, aes(x = reorder(tool, count), y = count, fill = tool)) +
      geom_col(show.legend = FALSE) +
      labs(title = "Web Frameworks", x = "Framework", y = "Usage Count") +
      theme_minimal() +
      coord_flip()
    ggplotly(p)
  })
  output$framework_dot <- renderPlotly({
    filtered <- webframework_filtered() %>% filter(tool %in% input$webframework_filter)
    if(nrow(filtered) == 0) {
      p <- ggplot() + labs(title = "Web Frameworks Dot Plot", x = "Usage Count", y = "Framework") + theme_minimal()
      return(ggplotly(p))
    }
    p <- ggplot(filtered, aes(x = count, y = reorder(tool, count))) +
      geom_point(size = 3, color = "forestgreen") +
      labs(title = "Web Frameworks Dot Plot", x = "Usage Count", y = "Framework") +
      theme_minimal()
    ggplotly(p)
  })
  output$framework_pie <- renderPlotly({
    filtered <- webframework_filtered() %>% filter(tool %in% input$webframework_filter)
    if(nrow(filtered) == 0) {
      return(plotly_empty(type = "pie", title = "Web Frameworks Pie Chart"))
    }
    plot_ly(filtered, labels = ~tool, values = ~count, type = 'pie',
            textinfo = 'label+percent', insidetextorientation = 'radial') %>%
      layout(title = "Web Frameworks Pie Chart")
  })
  # -------------------------
  # AI Developer Tools Visualizations
  # -------------------------
  output$ai_bar <- renderPlotly({
    filtered <- aitool_filtered() %>% filter(tool %in% input$aitool_filter)
    if(nrow(filtered) == 0) {
      p <- ggplot() + labs(title = "AI Developer Tools", x = "Tool", y = "Usage Count") + theme_minimal()
      return(ggplotly(p))
    }
    p <- ggplot(filtered, aes(x = reorder(tool, count), y = count, fill = tool)) +
      geom_col(show.legend = FALSE) +
      labs(title = "AI Developer Tools", x = "Tool", y = "Usage Count") +
      theme_minimal() +
      coord_flip()
    ggplotly(p)
  })
  output$ai_dot <- renderPlotly({
    filtered <- aitool_filtered() %>% filter(tool %in% input$aitool_filter)
    if(nrow(filtered) == 0) {
      p <- ggplot() + labs(title = "AI Developer Tools Dot Plot", x = "Usage Count", y = "Tool") + theme_minimal()
      return(ggplotly(p))
    }
    p <- ggplot(filtered, aes(x = count, y = reorder(tool, count))) +
      geom_point(size = 3, color = "purple") +
      labs(title = "AI Developer Tools Dot Plot", x = "Usage Count", y = "Tool") +
      theme_minimal()
    ggplotly(p)
  })
  output$ai_pie <- renderPlotly({
    filtered <- aitool_filtered() %>% filter(tool %in% input$aitool_filter)
    if(nrow(filtered) == 0) {
      return(plotly_empty(type = "pie", title = "AI Developer Tools Pie Chart"))
    }
    plot_ly(filtered, labels = ~tool, values = ~count, type = 'pie',
            textinfo = 'label+percent', insidetextorientation = 'radial') %>%
      layout(title = "AI Developer Tools Pie Chart")
  })
  # Render Raw Data Table
  output$raw_table <- renderDT({
    datatable(filtered_data_debounced(),
              extensions = 'Buttons',
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
              ))
  })
}
# Run the Shiny app
shinyApp(ui, server)
