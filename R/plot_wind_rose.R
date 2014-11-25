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

#' Plot wind rose
#' 
#' Plot wind rose
#' 
#' Plot wind rose
#' 
#' @param data : data.frame, with vitesse.name+point.grid and direction.name+point.grid columns. See example
#' @param point.grid : vector of numeric point of grid for first serie
#' @param data.add : If wanted to add a second serie on another data, second data.frame. NULL by default
#' @param point.grid.add : If wanted to add a second serie, vector of numeric point of grid. NULL by default. If not null and data.add is null, second serie is based on data
#' @param range.direction : range of direction values. c(0,360) by default.
#' @param ori.direction : Direction for the minimum value, and for the first 1/4 circle. Can be two of "W", "S", "N", "E", .... c("N","E") by default. N = 0, and E = 90
#' @param type : type of graphic & transformation. "rose" for classic wind rose, "quantile" to apply quantile per direction.
#' @param breaks.vitesse : breaks for direction transformation. Default to = c(0,0.5,2,4,6,8,10,Inf) for "cut" and c(0,0.05,0.25,0.5,0.75,0.95,1) for"quantile"
#' @param title : title, default to "Wind Rose" 
#' @param vitesse.name : name of vitesse columns in data. Default to "VITESSE"
#' @param direction.name : name of direction columns in data. Default to "DIRECTION"
#' @param col : vector of colors (html code rather). From strong wind to low wind. Default to c('#000000', '#0B4C5F', '#086A87','#01A9DB', '#2ECCFA', '#81DAF5', '#CEECF5', '#EFF8FB')
#' @param plot : If assign graphic to an R object, plot during calculation ? FALSE by default
#' @param height : height of output, default to 450
#' 
#' @return a Highcharts object
#' 
#' @examples
#' 
#' require(highchartsUtils)
#' data(windRoseData)
#' head(windRoseData)
#' 
#' # one serie, wind Rose (type = "rose", default value)
#' plotWindRose(data = windRoseData, point.grid = 1, title ="Wind Rose")
#' 
#' # change direction orientation
#' plotWindRose(data = windRoseData, point.grid = 1, ori.direction = c("N","W"))
#' 
#' # one serie, quantile
#' plotWindRose(data = windRoseData, point.grid = 1, type  = "quantile", title ="Wind Rose - quantile")
#' 
#' # change colors, keeping result
#' res <- plotWindRose(data = windRoseData, point.grid = 1, title ="Wind Rose", 
#'  col = c("#AC58FA","#8A0808", "#0B2161","#DF0174","#D0FA58"))
#' res
#' 
#' plotWindRose(data = windRoseData, point.grid = 1, title ="Wind Rose", 
#'  col = c("red","blue","green","yellow"))
#'  
#' # type = "rose", change breaks
#' plotWindRose(data = windRoseData, point.grid = 1, title ="Wind Rose - Change break", 
#'  breaks.vitesse = c(0,10,20,30,Inf))
#' 
#' # two series, on same data
#' plotWindRose(data = windRoseData, point.grid = 1, point.grid.add = 2:3, 
#'  breaks.vitesse = c(0,2,5,10,15,20,Inf))
#' 
#' # two series - two dataset, quantile per Direction, changing quantile
#' res <- plotWindRose(data = windRoseData, point.grid = 1, data.add = windRoseData[1:1000,], point.grid.add = 1,
#'  type = "quantile", breaks.vitesse = c(0.3, 0.5,0.8))
#'  
#' res
#' 
#' save.graphic(res,file = "Package_libraries_to_file.html")
#' save.graphic(res,file = "Internet_libraries_to_file.html", use.internet.library = TRUE)
#' save.graphic(res,file = "Package_libraries_linking_file.html", write.library.to.file = FALSE)
#' save.graphic(res,file = "Internet_libraries_linking_file.html", use.internet.library = TRUE, write.library.to.file = FALSE)
#' 
#' @export
#' @note Copyright 2014 RTE
# @include highchartsUtilsClass.R
# @include utils.R

# data = windRoseData
# point.grid = 1
# data.add = NULL
# point.grid.add = NULL
# range.direction = c(0,360)
# ori.direction = c("N","E")
# type = "quantile"
# breaks.vitesse = if(type == "rose"){c(0,0.5,2,4,6,8,10,Inf)}else{c(0.25,0.5,0.75)}
# title = "Wind Rose"
# vitesse.name = "VVENT"
# direction.name = "DVENT"
# col = c('#000000', '#0B4C5F', '#086A87','#01A9DB', '#2ECCFA', '#81DAF5', '#CEECF5', '#EFF8FB') 
# plot = FALSE
# height = 450

plotWindRose <- function(data, point.grid, data.add = NULL, point.grid.add = NULL, 
                         range.direction = c(0,360), ori.direction = c("N","E"), type = "rose", 
                         breaks.vitesse = if(type == "rose"){c(0,0.5,2,4,6,8,10,Inf)}else{c(0.25,0.5,0.75)},
                         title = "Wind Rose", vitesse.name = "VITESSE", direction.name = "DIRECTION",
                         col = c('#000000', '#0B4C5F', '#086A87','#01A9DB', '#2ECCFA', '#81DAF5', '#CEECF5', '#EFF8FB'), 
                         plot = FALSE, height = 450){
  
  if(!any(c("rose","quantile")%in%type)){
    stop("Available values for type is 'rose' or 'quantile'")
  }
  
  ##############################
  # bonnes directions ordonnees
  ##############################
  
  vector.direction <- c("W","WSW","SW","SSW","S","SSE","SE","ESE","E","ENE","NE","NNE","N","NNW","NW","WNW")
  
  if(any(!ori.direction%in%vector.direction)){
    stop("Invalid ori.direction, must be two of ", paste(vector.direction, collapse = " ,"))
  }
  
  ind.begin <- which(vector.direction == ori.direction[1])
  if(ind.begin > 1){
    labels.direction <- vector.direction[c(ind.begin:length(vector.direction), 1:(ind.begin-1))]
  }else{
    labels.direction <- vector.direction
  }
  
  
  if(which(labels.direction == ori.direction[2]) != 5){
    vector.direction <- rev(vector.direction)
    ind.begin <- which(vector.direction == ori.direction[1])
    if(ind.begin > 1){
      labels.direction <- vector.direction[c(ind.begin:length(vector.direction), 1:(ind.begin-1))]
    }else{
      labels.direction <- vector.direction
    }
  }
  
  ##############################
  # transformation des donnees
  ##############################
  
  data.wind <- computeDataWind(data, point.grid, vitesse.name, direction.name, range.direction, labels.direction, type, breaks.vitesse)
  
  res <- highchartsUtils::Highcharts$new()
  
  res$chart(polar = TRUE,  height = height)
  
  for(i in 2:ncol(data.wind)){
    res$series(data = toJSONArray2(data.wind[,c(1,i)], json = F, names = F), name = paste0("Serie 1",colnames(data.wind)[i]),
               type = ifelse(type == "rose", "column","area"), color = col[((i-2)%%length(col))+1], shadow = TRUE, lineWidth = 1.5,
               marker = list(fillColor = NULL, lineWidth = 2, lineColor = NULL, symbol = "circle", radius = 2))
  }
  
  
  if(!is.null(point.grid.add)){
    
    if(!is.null(data.add)){
      data.wind <- computeDataWind(data.add, point.grid.add, vitesse.name, direction.name, range.direction, labels.direction, type, breaks.vitesse)
    }else{
      data.wind <- computeDataWind(data, point.grid.add, vitesse.name, direction.name, range.direction, labels.direction, type, breaks.vitesse)
    }
    
    for(i in 2:ncol(data.wind)){
      res$series(data = toJSONArray2(data.wind[,c(1,i)], json = F, names = F), name = paste0("Serie 2",colnames(data.wind)[i]), 
                 type = "line", showInLegend = FALSE, color = col[((i-2)%%length(col))+1], shadow = TRUE, lineWidth = 1.5,
                 marker = list(fillColor = '#FFFFFF', lineWidth = 2, lineColor = NULL, symbol = "circle", radius = 2))
    }
    
    stacking <- NULL
    if(type == "rose"){
      stacking = "normal"
    }
    res$plotOptions(series = list(stacking = stacking, shadow = FALSE, groupPadding = 0, pointPlacement = 'on',
                                  events = list(legendItemClick = "#! function () {return false; } !#")))
  }else{
    stacking <- NULL
    if(type == "rose"){
      stacking = "normal"
    }
    res$plotOptions(series = list(stacking = stacking, shadow = FALSE, groupPadding = 0, pointPlacement = 'on'))
  }
  res$title(text = title)
  
  res$pane(size = '90%')
  
  res$legend(reversed = TRUE, align = 'right', verticalAlign  = 'top', y = 80,layout = 'vertical',
             labelFormatter = "#! function () {
             return this.name.substring(7);
} !#")
  
  
  res$xAxis(tickmarkPlacement = 'on', categories = as.character(data.wind[,1]))
  
  if(type == "rose"){
    res$yAxis(min = 0, endOnTick = FALSE, showLastLabel = TRUE,
              title = list(text = ''),labels = "#!{
              formatter: function () {
              return this.value + '%';
              }} !#")
    
    res$tooltip(shared = FALSE, valueSuffix = '%', formatter="#! function() {
              return this.series.name.substring(0,7) + '<br><b>' + this.x + '</b> : <b>' + this.series.name.substring(7) +'</b><br><b>' + this.y + '%</b>';
              }!#")
    
    }else{
      res$yAxis(min = 0, endOnTick = FALSE, showLastLabel = TRUE,
                title = list(text = ''),labels = "#!{
                formatter: function () {
                return this.value + ' m/s';
                }} !#")
      
      res$tooltip(shared = FALSE, valueSuffix = ' m/s', formatter="#! function() {
              return this.series.name.substring(0,7) + '<br><b>' + this.x + '</b> : <b>' + this.series.name.substring(7) +'</b><br><b>' + this.y + ' m/s</b>';
              }!#")
    }
  
  
  
  
  res$exporting(enabled = T)
  
  if(plot){
    res$show()
  }
  
  return(res)
  }



computeDataWind <- function(data, point.grid, vitesse.name, direction.name, range.direction, labels.direction, type, breaks.vitesse){
  
  data.graph <- do.call("rbind", lapply(point.grid, function(x){
    inter <- data[,c(paste0(c(vitesse.name, direction.name), x))]
    colnames(inter) <- c("vitesse","direction")
    inter
  }))
  
  data.graph$direction <- (data.graph$direction + range.direction[2]/32)%%range.direction[2]
  
  data.graph$Direction <- cut(round(data.graph$direction,4), breaks = round(seq(range.direction[1],range.direction[2], length.out = 17),4),
                              labels =labels.direction)
  
  if(type == "rose"){
    if(length(breaks.vitesse) ==2){
      labels.vitesse <- paste(breaks.vitesse[1],"-", breaks.vitesse[2]," m/s", sep="")
    }else{
      labels.vitesse <- paste(c(paste("<",breaks.vitesse[2]),
                                if(length(breaks.vitesse)>3){paste(breaks.vitesse[2:(length(breaks.vitesse)-2)],
                                                                   breaks.vitesse[3:(length(breaks.vitesse)-1)], sep="-")},
                                paste(">",breaks.vitesse[length(breaks.vitesse)-1])), "m/s")
    }
    
    data.graph$Vitesse <- cut(data.graph$vitesse, breaks  = breaks.vitesse, right = FALSE, labels = labels.vitesse)
    
    data.wind <- as.data.frame(table(Direction = data.graph$Direction, Vitesse = data.graph$Vitesse))
    data.wind <- xtabs(Freq ~ ., data.wind)
    data.wind <- as.data.frame(unclass(data.wind))
    cont = sum(data.wind)
    
    data.wind <- round(data.wind / cont *100,2)
    
    data.wind <- data.frame(Direction = rownames(data.wind), data.wind)
    data.wind$Direction <- as.character(data.wind$Direction)
    
    colnames(data.wind)[-1] <- labels.vitesse
    data.wind <- data.wind[,c(1,ncol(data.wind):2)]
    
    data.wind <- data.wind[c("N","NNE","NE","ENE","E","ESE","SE","SSE","S",
                             "SSW","SW","WSW","W","WNW","NW","NNW"),]
    
  }else{
    
    inter.data.wind <- do.call("rbind",lapply(levels(data.graph$Direction), function(x){
      ind <- which(data.graph$Direction==x)
      round(quantile(data.graph$vitesse[ind], probs = breaks.vitesse, na.rm = T), 2)
    }))
    
    data.wind <- data.frame(Direction = levels(data.graph$Direction), inter.data.wind)
    colnames(data.wind)[-1] <- colnames(inter.data.wind)
    rownames(data.wind) <- data.wind$Direction
    
    data.wind <- data.wind[,c(1,ncol(data.wind):2)]
    
    data.wind <- data.wind[c("N","NNE","NE","ENE","E","ESE","SE","SSE","S",
                             "SSW","SW","WSW","W","WNW","NW","NNW"),]
    
  }
  return(data.wind)
}
