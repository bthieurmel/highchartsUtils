require(highchartsUtils)
shinyUI(bootstrapPage(
  
  # control size
  tags$head(
    tags$style('#show{width : 80% ; height: 500px;}')
  ),
  highchartsOutput("show", "highstock")
))