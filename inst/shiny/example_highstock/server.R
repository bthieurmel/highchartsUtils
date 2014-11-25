require(highchartsUtils)

shinyServer(function(input, output) {
  output$show <- renderHighcharts({

    data(timeSeriesData)
    data = timeSeriesData[1:500,]
    r <- plotTimeSeries(data = data, ind.date = 1, ind.time = 2)
    
    # needed same name as output to dom
    r$addParams(dom = "show")
    r
  })
})
