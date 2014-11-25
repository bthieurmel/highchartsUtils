library(highchartsUtils)

require(highchartsUtils)

res <- highchartsUtils::Highcharts$new()

res$chart(type = "pie", animation = FALSE)

res$xAxis(categories = "#!['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']!#")

res$series(data = c(29.9, 71.5, 106.4, 129.2, 144.0, 176.0, 135.6, 148.5, 216.4, 194.1, 95.6, 54.4))        

res$htmlHeader('<button id="button">Toggle Labels</button>')
res$addParams(dom="mydom")

res$functionsJS("  // the button action
  $('#button').click(function() {
    var chart = $('#mydom').highcharts();
    var opt = chart.series[0].options;
    opt.dataLabels.enabled = !opt.dataLabels.enabled;
    chart.series[0].update(opt);
    
  });")
res
