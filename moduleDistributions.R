moduleDistributions <- function(id) {
  ns <- NS(id)
  tabPanel("Distributions",
           fluidRow(
             column(6,
                    align = "center",
                    wellPanel(
                      tabsetPanel(id = ns("dist_tabs"),
                        tabPanel("Normal distribution",
                                 br(),
                                 shiny::sliderInput(ns("mean"),
                                                    label = "Choose a mean:",
                                                    min = -10,
                                                    max = 10,
                                                    step = 0.1,
                                                    value = 0),
                                 shiny::sliderInput(ns("variance"),
                                                    label = "Choose a variance:",
                                                    min = 0.1,
                                                    max = 10,
                                                    value = 1)),
                        tabPanel("Uniform distribution",
                                 br(),
                                 shiny::sliderInput(ns("a"),
                                                    label = "Choose a minimum:",
                                                    min = -15,
                                                    max = 15,
                                                    step = 1,
                                                    value = 0),
                                 shiny::sliderInput(ns("b"),
                                                    label = "Choose an maximum:",
                                                    min = -15,
                                                    max = 15,
                                                    step = 1,
                                                    value = 1)),
                        tabPanel("Exponential distribution",
                                 br(),
                                 shiny::sliderInput(ns("rate"),
                                                    label = "Choose a rate:",
                                                    min = 0.1,
                                                    max = 5,
                                                    step = 0.1,
                                                    value = 1))
                      ),
                      style = "background: white;")
             ),
             column(6,
                    plotOutput(ns("histogram")),
             )
           )
  )
}


distributionsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      output$histogram <- renderPlot({
        g <- ggplot(data = data.frame(x = seq(-20, 20, by = 0.001))) +
          labs(x = "X", y = "Density") +
          xlim(-20, 20) +
          ylim(0, 1.5) +
          coord_cartesian(expand = FALSE) +
          theme_minimal() +
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

        if (input$dist_tabs == "Normal distribution") {
          p = g +
            geom_area(stat = "function",
                      fun = dnorm,
                      args = list(mean = input$mean,
                                  sd = sqrt(input$variance),
                                  log = FALSE),
                      colour = "black",
                      fill = "#b20e10",
                      linewidth = 0.2,
                      xlim = c(-20, 20),
                      alpha = 0.7)
        }
        if (input$dist_tabs == "Uniform distribution") {
          if (input$b > input$a) {
            p = g +
              geom_area(stat = "function",
                        fun = dunif,
                        args = list(min = input$a,
                                    max = input$b),
                        colour = "black",
                        fill = "#b20e10",
                        linewidth = 0.2,
                        xlim = c(input$a, input$b),
                        alpha = 0.7)
          } else {
            p = g +
              geom_text(
                data = data.frame(x = 0,
                                  y = 1,
                                  label = "Maximum must be bigger than minimum."),
                mapping = aes(
                  x = x, y = y, label = label
                ),
                family = "roboto"
                )
          }

        }
        if (input$dist_tabs == "Exponential distribution") {
          p = g +
            geom_area(stat = "function",
                      fun = dexp,
                      args = list(rate = input$rate,
                                  log = FALSE),
                      colour = "black",
                      fill = "#b20e10",
                      linewidth = 0.2,
                      xlim = c(0, 20),
                      alpha = 0.7)
        }
        return(p)
      })

    }
  )
}
