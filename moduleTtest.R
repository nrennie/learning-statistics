moduleTtest <- function(id) {
  ns <- NS(id)
  tabPanel(
    "T-tests",
    fluidRow(
      column(6,
             align = "center",
             wellPanel(
               tabsetPanel(
                 tabPanel("One sample t-test"),
                 tabPanel("Two sample t-test"),
                 tabPanel("Paired t-test")
               ),
               style = "background: white;"
             )
      ),
      column(
        6
      )
    )
  )
}


ttestServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

    }
  )
}
