require(highchartsUtils)
shinyUI(bootstrapPage(
  
  # control size
  tags$head(
    tags$style('#show{width : 40% ; height: 500px;}')
  ),
  highchartsOutput("show")
))