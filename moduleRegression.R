moduleRegression <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Linear Regression",
    fluidRow(
      column(6,
        align = "center",
        wellPanel(
          shiny::h3("Linear regression"),
          shiny::tags$div(
            shiny::HTML("<i>y</i> = m<i>x</i> + c + &epsilon; where	&epsilon; ~ N(0, &sigma;<sup>2</sup>)<br><br>")
          ),
          shiny::sliderInput(ns("m_param"),
            label = "Choose a value of m:",
            min = -5,
            max = 5,
            step = 1,
            value = 0
          ),
          shiny::sliderInput(ns("c_param"),
            label = "Choose a value of c:",
            min = -2,
            max = 2,
            step = 0.25,
            value = 0
          ),
          shiny::sliderInput(ns("variance"),
                             label = "Choose a variance:",
                             min = 1,
                             max = 10,
                             step = 1,
                             value = 1),
          style = "background: white;"
        )
      ),
      column(
        6,
        plotOutput(ns("scatter")),
      )
    )
  )
}


regressionServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$scatter <- renderPlot({
        x <- seq(-2, 2, by = 0.05)
        y <- input$m_param * x + input$c_param +
          rnorm(length(x), mean = 0, sd = sqrt(input$variance))
        suppressWarnings(ggplot(
          data = data.frame(x = x, y = y),
          mapping = aes(
            x = x,
            y = y
          ),
        ) +
          geom_point(
            colour = alpha("#b20e10", 0.7),
            size = 1.5,
          ) +
          geom_smooth(
            method = "lm",
            formula = y ~ x,
            se = FALSE,
            colour = "black"
          ) +
          labs(x = "X", y = "Y") +
          xlim(-2.5, 2.5) +
          ylim(-20, 20) +
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
    }
  )
}
