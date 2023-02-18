server <- function(input, output, session) {
  
  # call modules
  descriptiveServer("descriptive")
  distributionsServer("distributions")

}

server
