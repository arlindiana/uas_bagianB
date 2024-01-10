# Load library
library(tidyr)
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(rstatix)
library(plotly)

# data
data <- data.frame(
  day = 1:10,
  left_sidebar = c(2.5, 2.7, 2.8, 2.6, 3.0, 2.4, 2.9, 2.5, 2.6, 2.7),
  center_page = c(3.8, 3.5, 4.0, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9),
  right_sidebar = c(3.1, 2.9, 3.0, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "CTR Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Overview", tabName = "data",icon = icon("table")),
      menuItem("Statistical Analysis", tabName = "statistical_analysis",icon = icon("cog")),
      menuItem("Visualization", tabName = "visualization",icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "data",
        h2("Tabel Data"),
        fluidRow(
          column(
            width = 6,
            DTOutput("data_table"),
            actionButton("hapus_data_btn", "Hapus Data Terpilih")
          ),
          box(
            title = "Tambah Data",
            status = "primary",
            solidHeader = TRUE,
            textInput("day_input", "Day:", ""),
            numericInput("left_sidebar_input", "Left Sidebar:", ""),
            numericInput("center_page_input", "Center Page:", ""),
            numericInput("right_sidebar_input", "Right Sidebar:", ""),
            actionButton("submit_btn", "Tambah Data")
          )
        )
      ),
      tabItem(
        tabName = "statistical_analysis",
        h2("Statistical Analysis"),
        column(
          width = 6,
        verbatimTextOutput("output_anova")
        ),
        box(
          title = "Conclusion",
          solidHeader = TRUE,
          status = "primary",
          htmlOutput("conclusion")
        )
      ),
      tabItem(
        tabName = "visualization",
        h2("Visualization"),
        plotlyOutput("barplot_visualisasi")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Inisialisasi data default
  rv <- reactiveValues(data = data, selected_rows = NULL, editing_row = NULL)
  
  # Menampilkan data tabel
  output$data_table <- renderDT({
    datatable(rv$data, editable = TRUE, selection = "multiple")
  })
  
  # Menambahkan data baru ke tabel
  observeEvent(input$submit_btn, {
    new_row <- data.frame(
      day = nrow(rv$data) + 1,
      left_sidebar = as.numeric(input$left_sidebar_input),
      center_page = as.numeric(input$center_page_input),
      right_sidebar = as.numeric(input$right_sidebar_input)
    )
    rv$data <- rbind(rv$data, new_row)
  })
  
  # Mengatur baris yang dipilih
  observeEvent(input$data_table_rows_selected, {
    rv$selected_rows <- input$data_table_rows_selected
  })
  
  # Hapus data terpilih
  observeEvent(input$hapus_data_btn, {
    if (!is.null(rv$selected_rows)) {
      rv$data <- rv$data[-rv$selected_rows, ]
      rv$selected_rows <- NULL
    }
  })
  
  # Statistical Analysis
  output$output_anova <- renderPrint({
    if (is.null(rv$data)) return(NULL)  
    cat("Data Summary:\n")
    print(summary(rv$data))
    
    cat("\nANOVA Test Results:\n")
    result_anova <- aov(cbind(left_sidebar, center_page, right_sidebar) ~ day, data = rv$data)
    print(summary(result_anova))
  })
  
  # Menampilkan penjelasan hasil ANOVA
  output$conclusion <- renderText({
    if (is.null(rv$data)) return(NULL)
    
    result_anova_left <- aov(left_sidebar ~ day, data = rv$data)
    result_anova_center <- aov(center_page ~ day, data = rv$data)
    result_anova_right <- aov(right_sidebar ~ day, data = rv$data)
    
    p_value_left <- format(summary(result_anova_left)[[1]]$'Pr(>F)'[1], digits = 3)
    p_value_center <- format(summary(result_anova_center)[[1]]$'Pr(>F)'[1], digits = 3)
    p_value_right <- format(summary(result_anova_right)[[1]]$'Pr(>F)'[1], digits = 3)
    
    result_text <- paste(
      "<strong>Left Sidebar</strong><br>",
      ifelse(as.numeric(p_value_left) > 0.05,
             paste("Dengan tingkat kepercayaan 95%, didapatkan p-value sebesar ", p_value_left, " > 0.05, artinya tidak terdapat perbedaan signifikan antar kelompok left sidebar dan kelompok lainnya"),
             paste("Dengan tingkat kepercayaan 95%, didapatkan p-value sebesar", p_value_left, ") < 0.05, ada perbedaan signifikan untuk kelompok left sidebar dan kelompok lainnya")),
      "<br><br><strong>Center Page</strong><br>",
      ifelse(as.numeric(p_value_left) > 0.05,
             paste("Dengan tingkat kepercayaan 95%, didapatkan p-value sebesar", p_value_center, ") > 0.05, artinya tidak terdapat perbedaan signifikan antar kelompok center page dan kelompok lainnya"),
             paste("Dengan tingkat kepercayaan 95%, didapatkan p-value sebesar", p_value_center, ") < 0.05, ada perbedaan signifikan untuk kelompok center page dan kelompok lainnya")),
      "<br><br><strong>Right Sidebar</strong><br>",
      ifelse(as.numeric(p_value_left) > 0.05,
             paste("Dengan tingkat kepercayaan 95%, didapatkan p-value sebesar", p_value_right, ") > 0.05, artinya tidak terdapat perbedaan signifikan antar kelompok right sidebar dan kelompok lainnya"),
             paste("Dengan tingkat kepercayaan 95%, didapatkan p-value sebesar", p_value_right, ") < 0.05, ada perbedaan signifikan untuk kelompok right sidebar dan kelompok lainnya")),
      "<br><br><strong>Kesimpulan</strong><br>",
      "Berdasarkan hasil analisis, dengan tingkat kepercayaan 95% dapat disimpulkan bahwa tidak terdapat perbedaan signifikan dalam rata-rata Click-Through Rate (CTR) berdasarkan penempatan iklan pada website"
    )
    
    })
  # Visualisasi Barplot
  output$barplot_visualisasi <- renderPlotly({
    if (is.null(rv$data)) return(NULL)
    
    # Data untuk plot
    data_plot <- rv$data %>%
      pivot_longer(cols = c(left_sidebar, center_page, right_sidebar), names_to = "Group", values_to = "Value")
    # Plot
    p <- plot_ly(data_plot, x = ~Group, y = ~Value, type = "bar", color = ~Group) %>%
      layout(title = "Kinerja CTR berdasarkan Lokasi Penempatan Iklan",
             xaxis = list(title = "Location"),
             yaxis = list(title = "SUM CTR"),
             showlegend = TRUE)
    
    return(p)
  })
  
}

# Run the application
shinyApp(ui, server)