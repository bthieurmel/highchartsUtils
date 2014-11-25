require(highchartsUtils)


shinyServer(function(input, output) {
  output$show <- renderHighcharts({
    
    data(windRoseData)
    
    r <- plotWindRose(data = windRoseData, point.grid = 1, title ="Wind Rose")
    
    # needed same name as output to dom
    r$addParams(dom = "show")
    r
  })
})
