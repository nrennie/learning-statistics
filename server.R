server <- function(input, output, session) {

  # call modules
  descriptiveServer("descriptive")
  distributionsServer("distributions")
  confidenceServer("confidence")
  ttestServer("ttests")
  regressionServer("regression")

}

server
