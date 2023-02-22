moduleConfidence <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Confidence intervals",
    fluidRow(
      column(6,
        align = "center",
        wellPanel(
          shiny::sliderInput(ns("signif"),
            label = "Choose a significance level:",
            min = 0.80,
            max = 0.99,
            step = 0.01,
            value = 0.95
          ),
          shiny::sliderInput(ns("sample_size"),
            label = "Choose a sample size:",
            min = 0,
            max = 200,
            value = 40
          ),
          actionButton(ns("sample_button"), "New Sample"),
          div(style = "margin-bottom:10px;"),
          plotOutput(ns("people_plot")),
          style = "background: white;"
        )
      ),
      column(
        6,
        textOutput(ns("interval")),
        br(),
        plotOutput(ns("histogram"))
      )
    )
  )
}


confidenceServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      s <- eventReactive(input$sample_button, {
        as.numeric(Sys.time())
      })

      sample_df <- reactive({
        set.seed(s())
        sample_ids <- sample(1:200, size = input$sample_size, replace = FALSE)
        sample_df <- filter(population_df, ID %in% sample_ids)
        sample_df
      })

      output$people_plot <- renderPlot({
        ggplot() +
          geom_text(
            data = population_df,
            mapping = aes(
              x = x,
              y = y,
              label = fontawesome("fa-user")
            ),
            family = "fontawesome-webfont", size = 10, colour = "grey"
          ) +
          geom_text(
            data = sample_df(),
            mapping = aes(
              x = x,
              y = y,
              label = fontawesome("fa-user"),
              colour = Value
            ),
            family = "fontawesome-webfont", size = 10
          ) +
          scale_colour_gradient(low = "#d88687", high = "#7c090b", limits = c(10, 90)) +
          labs(title = glue::glue("Sample: {input$sample_size} people")) +
          theme_void() +
          theme(
            legend.position = "none",
            legend.title = element_blank(),
            plot.background = element_rect(fill = "transparent", colour = "transparent"),
            panel.background = element_rect(fill = "transparent", colour = "transparent"),
            plot.margin = margin(10, 10, 10, 10),
            plot.title = element_text(
              face = "bold",
              hjust = 0.5,
              family = "roboto",
              size = 20,
              margin = margin(b = 10)
            )
          )
      })

      ci <- reactive({
        x <- sample_df()$Value
        mu <- mean(x)
        s <- sd(x)
        n <- length(x)
        err <- qnorm(input$signif) * s / sqrt(n)
        ci <- c(mu - err, mu + err)
        return(ci)
      })

      output$interval <- renderText({
        paste0("Confidence interval: ", round(ci()[1], 2), " - ", round(ci()[2], 2))
      })

      output$histogram <- renderPlot({
        ggplot(data = sample_df()) +
          geom_histogram(
            mapping = aes(x = Value),
            colour = "black",
            fill = "#b20e10",
            linewidth = 0.2,
            alpha = 0.5,
            binwidth = 5,
          ) +
          geom_rect(
            data =
              data.frame(
                xmin = ci()[1],
                xmax = ci()[2],
                ymin = 0,
                ymax = 50
              ),
            mapping = aes(
              xmin = xmin,
              xmax = xmax,
              ymin = ymin,
              ymax = ymax
            ),
            fill = "#3E78B2",
            alpha = 0.7
          ) +
          geom_vline(
            xintercept = mean(sample_df()$Value),
            colour = "black"
          ) +
        labs(x = "X", y = "Count") +
          ylim(0, 50) +
          coord_cartesian(expand = FALSE, xlim = c(10, 90)) +
          theme_minimal() +
          theme(
            legend.position = "none",
            legend.title = element_blank(),
            plot.background = element_rect(fill = "transparent", colour = "transparent"),
            panel.background = element_rect(fill = "transparent", colour = "transparent"),
            plot.margin = margin(10, 10, 10, 10),
            plot.title = element_text(
              face = "bold",
              hjust = 0.5,
              family = "roboto",
              size = 20,
              margin = margin(b = 10)
            )
          )
      })
    }
  )
}
