moduleTtest <- function(id) {
  ns <- NS(id)
  tabPanel(
    "T-tests",
    fluidRow(
      column(6,
        align = "center",
        wellPanel(
          tabsetPanel(id = ns("ttest_tabs"),
            tabPanel(
              "One sample t-test",
              br(),
              shiny::sliderInput(ns("mean"),
                label = "Choose a mean to test against:",
                min = -2,
                max = 2,
                step = 0.25,
                value = 0
              ),
              shiny::sliderInput(ns("signif_one"),
                label = "Choose a significance level:",
                min = 0.80,
                max = 0.99,
                step = 0.01,
                value = 0.95
              ),
              plotOutput(ns("hist_one"))
            ),
            tabPanel("Two sample t-test",
                     br(),
                     shiny::sliderInput(ns("diff"),
                                        label = "Choose a difference to test:",
                                        min = -2,
                                        max = 2,
                                        step = 0.25,
                                        value = 0
                     ),
                     shiny::sliderInput(ns("signif_two"),
                                        label = "Choose a significance level:",
                                        min = 0.80,
                                        max = 0.99,
                                        step = 0.01,
                                        value = 0.95
                     ),
                     plotOutput(ns("hist_two"))),
            tabPanel("Paired t-test",
                     br(),
                     shiny::sliderInput(ns("diff_pair"),
                                        label = "Choose a difference to test:",
                                        min = -2,
                                        max = 2,
                                        step = 0.25,
                                        value = 0
                     ),
                     shiny::sliderInput(ns("signif_three"),
                                        label = "Choose a significance level:",
                                        min = 0.80,
                                        max = 0.99,
                                        step = 0.01,
                                        value = 0.95
                     ),
                     plotOutput(ns("hist_three")))
          ),
          style = "background: white;"
        )
      ),
      column(
        6,
        verbatimTextOutput(ns("ttest_output"))
      )
    )
  )
}


ttestServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      data_x <- reactive({
        set.seed(1234)
        x <- rnorm(50, 0, 3)
        x
      })

      data_y <- reactive({
        set.seed(5678)
        y <- rnorm(50, 0, 3)
        y
      })

      data_x_paired <- reactive({
        set.seed(9101112)
        y <- data_x() + rnorm(50, 0, 3)
        y
      })

      output$hist_one <- renderPlot({
        suppressWarnings(ggplot(data = data.frame(x = data_x())) +
          geom_histogram(
            mapping = aes(x = x),
            colour = "black",
            fill = "#b20e10",
            linewidth = 0.2,
            alpha = 0.7,
            bins = 30
          ) +
          geom_vline(
            xintercept = input$mean,
            colour = "black"
          ) +
          labs(x = "X", y = "Count") +
          xlim(-10, 10) +
          coord_cartesian(expand = FALSE) +
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
          ))
      })

      output$hist_two <- renderPlot({
        plot_data <- data.frame(
          x = data_x(),
          y = data_y()
          ) |>
          pivot_longer(cols = everything())

        suppressWarnings(ggplot(data = plot_data) +
                           geom_histogram(
                             mapping = aes(x = value,
                                           fill = name),
                             position = "identity",
                             colour = "black",
                             linewidth = 0.2,
                             alpha = 0.7,
                             bins = 30
                           ) +
                           scale_fill_manual(values = c("#b20e10", "#3E78B2")) +
                           labs(x = "Value of X or Y", y = "Count") +
                           xlim(-10, 10) +
                           coord_cartesian(expand = FALSE) +
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
                           ))
      })

      output$hist_three <- renderPlot({
        plot_data <- data.frame(
          Before = data_x(),
          After = data_x_paired()
        ) |>
          pivot_longer(cols = everything()) |>
          mutate(name = factor(name, levels = c("Before", "After")))

        suppressWarnings(ggplot(data = plot_data) +
                           geom_histogram(
                             mapping = aes(x = value,
                                           fill = name),
                             position = "identity",
                             colour = "black",
                             linewidth = 0.2,
                             alpha = 0.7,
                             bins = 30
                           ) +
                           facet_wrap(~name, nrow = 1) +
                           scale_fill_manual(values = c("#b20e10", "#3E78B2")) +
                           labs(x = "X", y = "Count") +
                           xlim(-10, 10) +
                           coord_cartesian(expand = FALSE) +
                           theme_minimal() +
                           theme(
                             legend.position = "none",
                             legend.title = element_blank(),
                             plot.background = element_rect(fill = "transparent", colour = "transparent"),
                             panel.background = element_rect(fill = "transparent", colour = "transparent"),
                             plot.margin = margin(10, 10, 10, 10),
                             panel.spacing = unit(2, "lines"),
                             strip.text = element_text(
                               family = "roboto",
                               size = 14,
                             ),
                             plot.title = element_text(
                               face = "bold",
                               hjust = 0.5,
                               family = "roboto",
                               size = 20,
                               margin = margin(b = 10)
                             )
                           ))
      })

      output$ttest_output <- renderPrint({
        if (input$ttest_tabs == "One sample t-test") {
          result <- t.test(x = data_x(),
                           mu = input$mean,
                           conf.level = input$signif_one,
                           var.equal = TRUE)
        }
        if (input$ttest_tabs == "Two sample t-test") {
          result <- t.test(x = data_x(),
                           y = data_y(),
                           mu = input$diff,
                           conf.level = input$signif_two,
                           var.equal = TRUE)
        }
        if (input$ttest_tabs == "Paired t-test") {
          result <- t.test(x = data_x(),
                           y = data_x_paired(),
                           mu = input$diff_pair,
                           conf.level = input$signif_three,
                           paired = TRUE,
                           var.equal = TRUE)
        }
        result
      }
      )


    }
  )
}
