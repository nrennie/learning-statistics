ui <- navbarPage(title = "Learn statistics!",
                 header = includeCSS("www/styles.css"),
                 # Descriptive statistics tab
                 moduleDescriptive("descriptive"),
                 # Distributions tab
                 moduleDistributions("distributions"),
                 # Regression tab
                 moduleRegression("regression")
                 )

ui
