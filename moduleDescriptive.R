moduleDescriptive <- function(id) {
  ns <- NS(id)
  tabPanel("Descriptive Statistics",
           fluidRow(
             column(6,
                    align = "center",
                    wellPanel(
                      shiny::sliderInput(ns("sample_size"),
                                         label = "Choose a sample size:",
                                         min = 0, 
                                         max = 200,
                                         value = 40),
                      actionButton(ns("sample_button"), "New Sample"),
                      div(style = "margin-bottom:10px;"),
                      plotOutput(ns("people_plot")),
                      style = "background: white;")
             ),
             column(6,
                    reactableOutput(ns("desc_stats_table"))
             )
           )
  )
}


descriptiveServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      s = eventReactive(input$sample_button, {
        as.numeric(Sys.time())
      })
      
      sample_df <- reactive({
        set.seed(s())
        sample_ids = sample(1:200, size = input$sample_size, replace = FALSE)
        sample_df =  filter(population_df, ID %in% sample_ids)
        sample_df
      })
      
      output$people_plot <- renderPlot({
        ggplot() +
          geom_text(data = population_df,
                    mapping = aes(x = x,
                                  y = y,
                                  label = fontawesome('fa-user')),
                    family='fontawesome-webfont', size = 10, colour = "grey") +
          geom_text(data = sample_df(),
                    mapping = aes(x = x,
                                  y = y,
                                  label = fontawesome('fa-user'),
                                  colour = Value),
                    family='fontawesome-webfont', size = 10) +
          scale_colour_gradient(low = "#d88687", high = "#7c090b", limits = c(20, 80)) +
          labs(title = glue::glue("Sample: {input$sample_size} people")) +
          theme_void() +
          theme(legend.position = "none", 
                legend.title = element_blank(),
                plot.background = element_rect(fill = "transparent", colour = "transparent"),
                panel.background = element_rect(fill = "transparent", colour = "transparent"),
                plot.margin = margin(10, 10, 10, 10), 
                plot.title = element_text(face = "bold",
                                          hjust = 0.5,
                                          family = "roboto",
                                          size = 20,
                                          margin = margin(b = 10)))
        
      })
      
      output$desc_stats_table <- renderReactable({
        stats <- c("Mean", "Median", "Mode", "Range (minimum)",
                   "Range (maximum)", "Variance", "Standard Deviation")
        values_vec <- sample_df()$Value
        values <- c(round(mean(values_vec), 2),
                    median(values_vec),
                    mode_avg(values_vec),
                    is_inf(suppressWarnings(min(values_vec))),
                    is_inf(suppressWarnings(max(values_vec))),
                    round(var(values_vec), 2),
                    round(sd(values_vec), 2))
        stats_table <- tibble(Statistic = stats, Value = values)
        reactable(stats_table,
                  defaultColDef = colDef(
                    align = "center"
                  ))
      })
      
    }
  )
}