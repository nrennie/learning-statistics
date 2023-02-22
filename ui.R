ui <- navbarPage(title = "Learn statistics!",
                 header = includeCSS("www/styles.css"),
                 # Descriptive statistics tab
                 moduleDescriptive("descriptive"),
                 # Distributions tab
                 moduleDistributions("distributions"),
                 # Confidence intervals tab
                 moduleConfidence("confidence"),
                 # t-test tab
                 moduleTtest("ttests"),
                 # Regression tab
                 moduleRegression("regression")
                 )

ui
