# This file is part of highchartsUtils R package. https://github.com/bthieurmel/highchartsUtils
# 
# highchartsUtils is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# highchartsUtils is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with highchartsUtils  If not, see http://www.gnu.org/licenses/.
#
# The JavaScript charting libraries that are included with this package are licensed under their own terms.
# They are free for non-commercial use, but not free for commercial use. 

#' Plot time series
#' 
#' Plot time series
#' 
#' Plot time series
#' 
#' @param data : data.frame
#' @param ind.date : indice of data column
#' @param ind.time : indice of time column, if NULL, on take date column
#' @param ind.curve : time series to plot. Default to if(!is.null(data)){3:ncol(data)}else{NULL}
#' @param title : Title. Optional ,"" by default
#' @param subtitle : Subtitle. Optional ,"" by default
#' @param xlab : x label. Optional ,"" by default
#' @param ylab : y label. Optional ,"" by default
#' @param zoomType : zoomType one of "x" (default), "y", "xy", or ""
#' @param lwd : line width. Optional, 1 by default
#' @param cex : marker size. Optional, 1 by default
#' @param type : type of graphic. one of "line" (default), "spline", or "column"
#' @param suffixe.value : add a suffixe on y legend ? "" by default
#' @param approximation : approximation used during aggregation. one of "average" (default), "high", "low", or "sum"
#' @param dash.style : like lty R, value(s) in  c("solid","shortdash","shortdot","shortdashdot","shortdashdotdot","dot","dash","longlash","dashdot","longdashdot", "longdashdotdot")
#' @param enabled.marker : See arkers on graphic ? TRUE by default
#' @param marker : marker type, value(s) in c("circle", "square", "diamond", "triangle","triangle-down")
#' @param height : height of graphic. Default to 500
#' @param tz : time zone of date/time, used in graphic transformation. "UTC" by default
#' @param plot : If assign graphic to an R object, plot during calculation ? FALSE by default
#' @param col : color(s) of curve(s). can be some R colors or html colors. Default to c('#2f7ed8', '#0d233a', '#8bbc21', '#910000', '#1aadce',
#'  '#492970', '#f28f43', '#77a1e5', '#c42525', '#a6c96a')
#' @param legend.enabled : Add a legend ? Default to TRUE
#' @param legend.layout : layout legend. "vertical" (Default) or "horizontal"
#' @param legend.halign : horizontal alignment "left" (Default), "center", or "right"
#' @param legend.valign : vertical alignment "middle" (Default), "top" or "bottom"
#' 
#' @return a Highcharts object
#' 
#' @examples
#' 
#'data(timeSeriesData)
#'head(timeSeriesData)
#'
#'
#'# simple plot
#'data = timeSeriesData[1:500,]
#'plotTimeSeries(data = data, ind.date = 1, ind.time = 2)
#'
#'# customize 
#'plotTimeSeries(data = data, ind.date = 1, ind.time = 2, cex = 3, marker = "diamond", type = "line", height = 600,
#'                 title = "My Time Series",subtitle = "Customize !", xlab = "Time", ylab = "Puissance",
#'                 suffixe.value = "W", col = "green", legend.halign = "center", legend.valign= "bottom")
#'                 
#'# plot, keep, and save
#'graph <- plotTimeSeries(data = data, ind.date = 1, ind.time = 2, plot = TRUE)
#'save.graphic(graph,"mysavetimeseries.html")
#'
#'# keep, and save, no plot
#'graph <- plotTimeSeries(data = data, ind.date = 1, ind.time = 2, col = "green")
#'save.graphic(graph,"mysavetimeseries2.html")
#'
#'# plot two daily time series
#'don = data.frame(date = seq(as.Date("1999/1/1"), as.Date("1999/12/31"), "days"), Value1 = rnorm(365,50,10), Value2 = rnorm(365,50,10))
#'head(don)
#'
#'plotTimeSeries(don, ind.date = 1, ind.curve = c(2:3), type = "spline", suffixe.value = "W", col = c("green","red"), plot = FALSE)
#' 
#' @export plotTimeSeries
#' 
#' @include highchartsUtilsClass.R
# @include utils.R
plotTimeSeries <- function(data, ind.date, ind.time = NULL, ind.curve = if(!is.null(data)){3:ncol(data)}else{NULL}, 
                                   title = "",  subtitle = "", xlab = "", ylab = "", zoomType = "x", lwd = 1, cex = 1,
                                   type = "line", suffixe.value="", approximation = "average",
                                   dash.style = c("solid","shortdash","shortdot","shortdashdot","shortdashdotdot","dot","dash","longlash","dashdot","longdashdot", "longdashdotdot"),
                                   enabled.marker = TRUE, marker = c("circle", "square", "diamond", "triangle","triangle-down"),
                                   width = "100%", height = 500, tz = "UTC",
                                   plot = FALSE, col = c('#2f7ed8', '#0d233a', '#8bbc21', '#910000', '#1aadce', 
                                   '#492970', '#f28f43', '#77a1e5', '#c42525', '#a6c96a'),
                                   legend.enabled = TRUE, legend.layout = "vertical", legend.halign = "left",legend.valign = "middle"){
  

  res <- highchartsUtils::Highstock$new()
  
  if(!is.null(data)){
    
    if(nrow(data) > 10000){
      warning("More than 10 000 rows, only print 1 to 10 000.")
      data <- data[1:10000,]
    }
    if(!is.null(ind.time)){
      date <- as.numeric(as.POSIXct(data[, ind.date], data[,ind.time], origin="1970-01-01", tz = tz)) * 1000
    }else{
      date <- as.numeric(as.POSIXct(data[, ind.date], origin="1970-01-01", tz = tz)) * 1000
    }
    
    data <- data.frame(date, data[,ind.curve,drop = FALSE])
        
    res$chart(type = type, zoomType = zoomType)
    
    res$title(text=title)
    
    res$subtitle(text=subtitle)
    
    for(i in 1:(ncol(data)-1)){
      res$series(data = toJSONArray2(data[,c(1,i+1)], json = F, names = F),
                 name = colnames(data)[i+1], 
                 dashStyle = dash.style[ifelse(i%%length(dash.style) == 0, length(dash.style), i%%length(dash.style))], 
                 marker = list(symbol = marker[ifelse(i%%length(marker) == 0, length(marker), i%%length(marker))], radius = cex[ifelse(i%%length(cex) == 0, length(cex), i%%length(cex))]),
                 lineWidth = lwd[ifelse(i%%length(lwd) == 0, length(lwd), i%%length(lwd))], 
                 color = col[ifelse(i%%length(col) == 0, length(col), i%%length(col))], 
                 dataGrouping = list(approximation = approximation, 
                                     units = "#! [[
                                       'millisecond', // unit name
                                       [1, 2, 5, 10, 20, 25, 50, 100, 200, 500] // allowed multiples
                                       ], [
                                         'second',
                                         [1, 2, 5, 10, 15, 30]
                                         ], [
                                           'minute',
                                           [1,10,30]
                                           ], [
                                             'hour',
                                             [1,3,6,12]
                                             ], [
                                               'day',
                                               [1]
                                               ], [
                                                 'week',
                                                 [1]
                                                 ], [
                                                   'month',
                                                   [1, 3, 6]
                                                   ], [
                                                     'year',
                                                     null
                                                     ]] !#"
                 ))
    }
    
    
    res$xAxis(title = list(text = xlab))
    
    res$yAxis(title = list(text = ylab), labels = list(format = paste('{value}', suffixe.value)))
    
    res$plotOptions(
      series = list(
        marker = list(enabled = enabled.marker)
      )
    )
    

    
    res$rangeSelector(enabled = TRUE ,buttons = "#! [
{
                      type: 'week',
                      count: 1,
                      text: '1w'
}, {
                      type: 'month',
                      count: 1,
                      text: '1m'
},{
                      type: 'month',
                      count: 3,
                      text: '3m'
},{
                      type: 'month',
                      count: 6,
                      text: '6m'
}, {
                      type: 'all',
                      text: 'All'
}]!#",replace = T)
    
    res$tooltip(followTouchMove = FALSE, shared = FALSE, replace = T, valueDecimals = 0)
    
    res$addParams(height = height)
    
    res$legend(layout = legend.layout, align = legend.halign, verticalAlign = legend.valign, enabled = legend.enabled)
    
    res$exporting(enabled = T)
    
    if(plot){
      res$show()
    }
    
  }
  return(res)
  
}
