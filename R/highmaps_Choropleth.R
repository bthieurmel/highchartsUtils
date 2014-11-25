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

#' Plot Interactive map using Highmaps
#' 
#' Plot Interactive map using Highmaps. France (regions) with or without dom, Europe and UE (countries)
#' 
#' Plot Interactive map using Highmaps. France (regions) with or without dom, Europe and UE (countries)
#' 
#' @param data : data.frame
#' @param value : column name of quantitative/qualitative value 
#' @param key : column name of key value to identify regions / countries. We have to use "hc-key" reference for identification. 
#' See data(Europe), data(UE), data(France_regions) and data(France_regions_dom) for reference table.
#' @param time : if wanted to create a animated map, column name of time value, default to NULL (no time)
#' @param title : title, default to ""
#' @param scope : scope of the map, one of "Europe", "UE", "France_regions" (default) or "France_regions_dom"
#' @param col : vector of colors (html code rather). Two colors for quantitative variable, n colors for qualitative variable, n = number of unique values
#' @return a Highmaps object
#' 
#' @examples
#' 
#' #------------   UE EXAMPLE, quantitative, not time ------------------#
#' 
#' require(highchartsUtils)
#' data(UE)
#' head(UE)
#' UE$value <- rnorm(nrow(UE), 50,100)
#' 
#' res <- highmapsChoropleth(UE, value ="value", key = "hc-key", title = "Exemple UE", scope = "UE")
#' res
#' 
#' # changing colors 
#' res <- highmapsChoropleth(UE, value ="value", key = "hc-key", title = "Exemple UE", scope = "UE", 
#'  col = c("#FBEFFB","#380B61"))
#' res
#' 
#' #------------   EUROPE EXAMPLE, quantitative, with time ------------------#
#' 
#' require(highchartsUtils)
#' data(Europe)
#' head(Europe)
#' 
#' Europe <- rbind(cbind(Europe, time = "01-01-2010"), cbind(Europe, time = "02-01-2010"), 
#'  cbind(Europe, time = "03-01-2010"), cbind(Europe, time = "04-01-2010"))
#' 
#' Europe$value <- rnorm(nrow(Europe),50,100)
#' 
#' res <- highmapsChoropleth(Europe, value ="value", key = "hc-key", time = "time", title = "Exemple Europe", scope = "Europe")
#' res
#' 
#' #------------   FRANCE EXAMPLE, qualitative, not time ------------------#
#' 
#' require(highchartsUtils)
#' data(France_regions)
#' head(France_regions)
#' France_regions$value <- as.factor(round(runif(nrow(France_regions))*4,0))
#' 
#' res <- highmapsChoropleth(France_regions, value ="value", key = "hc-key", title = "Exemple France", scope = "France_regions")
#' res
#' 
#' # changing colors 
#' 
#' res <- highmapsChoropleth(France_regions, value ="value", key = "hc-key", title = "Exemple France", scope = "France_regions", 
#'  col = c("red","green","blue","black", "yellow"))
#' res
#' 
#' #------------   FRANCE with DOM EXAMPLE, qualitative, time ------------------#
#' 
#' require(highchartsUtils)
#' data(France_regions_dom)
#' head(France_regions_dom)
#' 
#' France_regions_dom <- rbind(cbind(France_regions_dom, time = "first"), cbind(France_regions_dom, time = "second"), 
#'  cbind(France_regions_dom, time = "third"), cbind(France_regions_dom, time = "fourth"))
#' 
#' France_regions_dom$value <- as.factor(round(runif(nrow(France_regions_dom))*4,0))
#' 
#' res <- highmapsChoropleth(France_regions_dom, value ="value", key = "hc-key", time = "time", title = "Exemple France", scope = "France_regions")
#' res
#'
#' 
#' save.graphic(res,file = "Package_libraries_to_file.html")
#' save.graphic(res,file = "Internet_libraries_to_file.html", use.internet.library = TRUE)
#' save.graphic(res,file = "Package_libraries_linking_file.html", write.library.to.file = FALSE)
#' save.graphic(res,file = "Internet_libraries_linking_file.html", use.internet.library = TRUE, write.library.to.file = FALSE)
#' 
#' @include Highmaps.R
#' @export highmapsChoropleth
#' 

highmapsChoropleth <- function(data, value, key, time = NULL, title = "", scope = "France_regions", 
                               col = if(is.character(data$value) | is.factor(data$value)){
                                 c('#2f7ed8', '#0d233a', '#8bbc21', '#910000', '#1aadce', '#492970', '#f28f43', '#77a1e5', '#c42525', '#a6c96a')
                                }else{
                                 c("#FAFAFA","#0A0A2A")
                                }){
  
  serie.name <- value
  
  if(any(!c(time, key, value)%in%colnames(data))){
    stop("Missing ",paste(c(time, key, value)[!c(time, key, value)%in%colnames(data)], collapse = ", "), " in data")
  }
  
  
  res <- highchartsUtils::Highmaps$new()
  
  res$colors(col)
  res$setScope(scope)
  
  res$mapNavigation(enabled = TRUE,
                    buttonOptions = list(verticalAlign =  'bottom'))
  
  
  
  current.maps <- switch(scope,
                         France_regions = "countries/fr/custom/fr-all-mainland",
                         France_regions_dom = "countries/fr/fr-all",
                         Europe = "custom/europe",
                         UE = "custom/european-union")
  
  if(!is.null(time)){
    data <- data[,c(time, value, key)]
    
    colnames(data) <- gsub(value,"value", colnames(data))
    colnames(data) <- gsub(key,"hc-key", colnames(data))
    
    if(is.character(data$value) | is.factor(data$value)){
      
      res$plotOptions(map = list(allAreas = FALSE, joinBy ='hc-key', mapData = paste0("#! Highcharts.maps['", current.maps, "'] !#")))
      
      if(is.character(data$value)){
        unique.value <- unique(data$value)
      }else{
        unique.value <- levels(data$value)
      }
      
      res$levels(unique.value)
      
      controle <- length(unique(data[,time]))
      don = list()
      cpt = 1
      for(j in 1:controle){
#         don2= list()
        int = data[data[,time]==unique(data[,time])[j],]
#         cpt = 1
        for(k in unique.value){
          don3 = list()
          int2 <- int[int$value == k,,drop = FALSE]
          if(nrow(int2) > 0){
            for(i in 1:nrow(int2)){
              inter = list()
              inter[[1]] = int2[i,"hc-key"]
              inter[[2]] = int2[i,"value"]
              names(inter) <- c("hc-key", "value")
              don3[[i]] = inter
            }
          }else{
            don3 = NULL
          }
          
          don[[cpt]] = don3
          cpt = cpt + 1
        }
        

      }
#       for(j in 1:controle){
#         don2= list()
#         int = data[data[,time]==unique(data[,time])[j],]
#         cpt = 1
#         for(k in unique.value){
#           don3 = list()
#           int2 <- int[int$value == k,,drop = FALSE]
#           for(i in 1:nrow(int2)){
#             inter = list()
#             inter[[1]] = int2[i,"hc-key"]
#             inter[[2]] = int2[i,"value"]
#             names(inter) <- c("hc-key", "value")
#             don3[[i]] = inter
#           }
#           don2[[cpt]]= don3
#           cpt = cpt + 1
#         }
#         don[[j]] = don2
#       }

        
      ldata = plyr::alply(don, 1, as.list)
      names(ldata) <- paste(rep(1:controle, each = length(unique.value)), rep(unique.value, controle), sep = "-")

      lrefdata= as.list(unique(data[,time]))
      lrefdata=plyr::alply(lrefdata, 1, as.list)
          
      res$chartData(toJSON(ldata))
      
      res$RefchartData(toJSON(lrefdata))
      
      for(i in 1:length(unique.value)){
        res$series(name = unique.value[i],data = don[[i]], dataLabels = list(enabled = TRUE, format =  '{point.name}'))

      }
      
    }else{
      controle <- length(unique(data[,time]))
      don = list()
      
      for(j in 1:controle){
        don2= list()
        int = data[data[,time]==unique(data[,time])[j],]
        for(i in 1:nrow(int)){
          inter = list()
          inter[[1]] = int[i,"hc-key"]
          inter[[2]] = int[i,"value"]
          names(inter) <- c("hc-key", "value")
          don2[[i]] = inter
        }
        don[[j]] = don2
      }
      
      ldata = plyr::alply(don, 1, as.list)
      
      lrefdata= as.list(unique(data[,time]))
      lrefdata=plyr::alply(lrefdata, 1, as.list)
      
      res$chartData(toJSON(ldata))
      
      res$RefchartData(toJSON(lrefdata))
      
      res$series(data = don[[1]] ,mapData = paste0("#! Highcharts.maps['", current.maps, "'] !#"),
                 joinBy ='hc-key',
                 name = serie.name,
                 states = list(  hover = list(color = '#BADA05')),
                 dataLabels = list(enabled = TRUE, format =  '{point.name}'))
      
      res$colorAxis(min = min(data[,"value"]), max = max(data[,"value"]), minColor = col[1], maxColor = col[2])
    }

  res$subtitle(text = unique(data[,time])[1])

  }else{
    data <- data[,c(value, key)]
    
    colnames(data) <- gsub(value,"value", colnames(data))
    colnames(data) <- gsub(key,"hc-key", colnames(data))
    
    if(is.character(data$value) | is.factor(data$value)){
      
      res$plotOptions(map = list(allAreas = FALSE, joinBy ='hc-key', mapData = paste0("#! Highcharts.maps['", current.maps, "'] !#")))
      
      if(is.character(data$value)){
        unique.value <- unique(data$value)
      }else{
        unique.value <- levels(data$value)
      }
      
      for(j in unique.value){
        don = list()
        int <- data[data$value == j,,drop = FALSE]
        for(i in 1:nrow(int)){
          inter = list()
          inter[[1]] = int[i,"hc-key"]
          inter[[2]] = int[i,"value"]
          names(inter) <- c("hc-key", "value")
          don[[i]] = inter
        }
        
        
        res$series(name = j,data = don, dataLabels = list(enabled = TRUE, format =  '{point.name}'))
      }
      
    }else{
      don = list()
      for(i in 1:nrow(data)){
        inter = list()
        inter[[1]] = data[i,"hc-key"]
        inter[[2]] = data[i,"value"]
        names(inter) <- c("hc-key", "value")
        don[[i]] = inter
      }
      
      res$series(data = don, mapData = paste0("#! Highcharts.maps['", current.maps, "'] !#"),
                 joinBy ='hc-key',
                 name = serie.name,
                 states = list(  hover = list(color = '#BADA05')),
                 dataLabels = list(enabled = TRUE, format =  '{point.name}'))
      
      res$colorAxis(min = min(data[,"value"]), max = max(data[,"value"]), minColor = col[1], maxColor = col[2])
    }

  }
  
  res$title(text = title)
  
  res$chart(height = 600)
  
  res$set(bodyattrs = "ng-app ng-controller='rChartsCtrl'")
  
  #res$addAssets(jshead = "https://ajax.googleapis.com/ajax/libs/angularjs/1.2.26/angular.min.js")
  
  if(!is.null(time)){
    if(is.character(data$value) | is.factor(data$value)){
      range_time <- c(1,length(unique(data[,time])))
      
      res$setTemplate(chartDiv = sprintf("\n<div id='{{chartId}}' class='rChart datamaps'></div> 
                                     <br></br>
                                       <div class='container' style = 'text-align:center'>
                                         <span>time : </span>
                                         <input id='slider' type='range' min=%s max=%s ng-model='time' ng-change='changeMap()' width=200>
                                         <button style='width:60px;height:30px' ng-click='prevMap()'>Prev.</button>
                                         <button style='width:60px;height:30px' ng-click='nextMap()' >Next</button>                                         
                                         <button style='width:60px;height:30px' ng-click='animateMap()'>Play</button>
                                         <button style='width:60px;height:30px' ng-click='pauseMap()' >Pause</button>
                                         <button style='width:60px;height:30px' ng-click='resetMap()'>Stop</button>
                                         <span>speed : </span>
                                         <input id='slider' type='range' min=0 max=2995 ng-model='changespeed' width=200>
                                         
                                         </div>      
                                         <script>\
                                         
                                         var ctrl = 'play';
                                         var play = 0;
                                         function rChartsCtrl($scope, $timeout){
                                         
                                         $scope.changespeed = 1000;
                                         $scope.time = %s;
                                         
                                         $scope.changeMap = function(){
                                         ctrl = 'change'
                                         var chart = $('#{{chartId}}').highcharts();
                                         chart.setTitle(null, { text: refdata[$scope.time]});
                                         cpt = 0;
  									                     for(lev in levels){
										                      chart.series[cpt].update({data: data[$scope.time+'-'+lev]});
										                      cpt = cpt + 1;
										                     }
                                         
                                         return;
                                         }
                                         
  										                  $scope.prevMap = function(){
                                             ctrl = 'change'
                                         var chart = $('#{{chartId}}').highcharts();
                                         if($scope.time > 1){
                                         $scope.time = $scope.time -1;
                                         chart.setTitle(null, { text: refdata[$scope.time]});
                                         cpt = 0;
                                         for(lev in levels){
                                         chart.series[cpt].update({data: data[$scope.time+'-'+lev]});
                                         cpt = cpt + 1;
                                         }
                                         }
                                         return;
                                         }
                                         
                                         $scope.nextMap = function(){
                                         ctrl = 'change'
                                         var chart = $('#{{chartId}}').highcharts();
                                         if($scope.time < Object.keys(refdata).length){
                                         $scope.time = $scope.time +1;
                                         chart.setTitle(null, { text: refdata[$scope.time]});
                                         cpt = 0;
                                         for(lev in levels){
                                         chart.series[cpt].update({data: data[$scope.time+'-'+lev]});
                                         cpt = cpt + 1;
                                         }
                                         }
                                         return;
                                         }

                                         $scope.animateMap = function(){
                                         if(ctrl == 'change'){  
                                         ctrl = 'play'
                                         $scope.time = $scope.time-1
                                         }
                                         if(ctrl == 'play'){
                                         play = 1
                                         if ($scope.time > (%s-1)){
                                         ctrl = 'end'
                                         play = 0
                                         return;
                                         }
                                         $scope.time += 1;
                                         var chart = $('#{{chartId}}').highcharts();
                                         chart.setTitle(null, { text: refdata[$scope.time]});
                                         cpt = 0;
    								                     for(lev in levels){
										                      chart.series[cpt].update({data: data[$scope.time+'-'+lev]});
										                      cpt = cpt + 1;
										                     }
                                         $timeout($scope.animateMap,  3000 - $scope.changespeed)
                                         }else{
                                         play = 0
                                         ctrl = 'play'
                                         return;
                                         }
                                         }
                                         
                                         $scope.pauseMap = function(){
                                         if(ctrl == 'play' && play == 1){
                                         ctrl = 'pause'
                                         return;
                                         }
                                         ctrl = 'play'
                                         return;
                                         }
                                         
                                         $scope.resetMap = function(){
                                         if(ctrl == 'end' || ctrl == 'play' & play == 0){
                                         ctrl = 'play'
                                         }else{
                                         ctrl = 'reset'
                                         }
                                         $scope.time = %s

                                         var chart = $('#{{chartId}}').highcharts();
                                         chart.setTitle(null, { text: refdata[$scope.time]});
                                         cpt = 0;
                                         for(lev in levels){
										                      chart.series[cpt].update({data: data[$scope.time+'-'+lev]});
										                      cpt = cpt + 1;
										                     }
                                         return;
                                         }
                                         
                                         }\
                                         </script>", 
                                     range_time[1], range_time[2],range_time[1], range_time[2], range_time[1]))
    }else{
      range_time <- c(1,length(ldata))
      
      res$setTemplate(chartDiv = sprintf("\n<div id='{{chartId}}' class='rChart datamaps'></div> 
                                         <br></br>
                                         <div class='container' style = 'text-align:center'>
                                         <span>time : </span>
                                         <input id='slider' type='range' min=%s max=%s ng-model='time' ng-change='changeMap()' width=200>
                                         <button style='width:60px;height:30px' ng-click='prevMap()'>Prev.</button>
                                         <button style='width:60px;height:30px' ng-click='nextMap()' >Next</button> 
                                         <button style='width:60px;height:30px' ng-click='animateMap()'>Play</button>
                                         <button style='width:60px;height:30px' ng-click='pauseMap()' >Pause</button>
                                         <button style='width:60px;height:30px' ng-click='resetMap()'>Stop</button>
                                         <span>speed : </span>
                                         <input id='slider' type='range' min=0 max=2995 ng-model='changespeed' width=200>
                                         
                                         </div>      
                                         <script>\
                                         
                                         var ctrl = 'play';
                                         var play = 0;
                                         function rChartsCtrl($scope, $timeout){
                                         
                                         $scope.changespeed = 1000;
                                         $scope.time = %s;
                                         
                                         $scope.changeMap = function(){
                                          ctrl = 'change'
                                          var chart = $('#{{chartId}}').highcharts();
                                          chart.setTitle(null, { text: refdata[$scope.time]});
                                          chart.series[0].update({data: data[$scope.time]});
                                         
                                          return;
                                         }
                                         
  										                    $scope.prevMap = function(){
                                             ctrl = 'change'
                                             var chart = $('#{{chartId}}').highcharts();
                                         if($scope.time > 1){
                                         $scope.time = $scope.time -1;
                                         chart.setTitle(null, { text: refdata[$scope.time]});
                                         chart.series[0].update({data: data[$scope.time]});
                                         }
                                         return;
                                         }
                                         
                                         $scope.nextMap = function(){
                                         ctrl = 'change'
                                         var chart = $('#{{chartId}}').highcharts();
                                         if($scope.time < Object.keys(refdata).length){
                                         $scope.time = $scope.time +1;
                                         chart.setTitle(null, { text: refdata[$scope.time]});
                                         chart.series[0].update({data: data[$scope.time]});
                                         }
                                         return;
                                         }

                                         $scope.animateMap = function(){
                                          if(ctrl == 'change'){  
                                            ctrl = 'play'
                                            $scope.time = $scope.time-1
                                          }
                                          if(ctrl == 'play'){
                                            play = 1
                                            if ($scope.time > (%s-1)){
                                              ctrl = 'end'
                                              play = 0
                                              return;
                                            }
                                          $scope.time += 1;
                                          var chart = $('#{{chartId}}').highcharts();
                                          chart.setTitle(null, { text: refdata[$scope.time]});
                                          chart.series[0].update({data: data[$scope.time]});
                                          $timeout($scope.animateMap,  3000 - $scope.changespeed)
                                         }else{
                                          play = 0
                                          ctrl = 'play'
                                          return;
                                         }
                                         }
                                         
                                         $scope.pauseMap = function(){
                                         if(ctrl == 'play' && play == 1){
                                         ctrl = 'pause'
                                         return;
                                         }
                                         ctrl = 'play'
                                         return;
                                         }
                                         
                                         $scope.resetMap = function(){
                                         if(ctrl == 'end' || ctrl == 'play' & play == 0){
                                         ctrl = 'play'
                                         }else{
                                         ctrl = 'reset'
                                         }
                                         $scope.time = %s
                                         var chart = $('#{{chartId}}').highcharts();
                                         chart.setTitle(null, { text: refdata[$scope.time]});
                                         chart.series[0].update({data: data[$scope.time]});
                                         return;
                                         }
                                         
                                         }\
                                         </script>", 
                                     range_time[1], range_time[2],range_time[1], range_time[2], range_time[1]))
    }
   
    
  }else{
    res$setTemplate(chartDiv = "\n<div id='{{chartId}}' class='rChart datamaps'></div> ")
  }

  return(res)
  
}


# highmapsChoropleth <- function(data, time, value, key, title = "", scope = "France_regions"){
# 
#   require(plyr)
#   
#   serie.name <- value
#   
#   data <- data[,c(time, value, key)]
#   
#   colnames(data) <- gsub(value,"value", colnames(data))
#   colnames(data) <- gsub(key,"hc-key", colnames(data))
#   
#   controle <- length(unique(data[,time]))
#   don = list(controle)
#   
#   for(j in 1:controle){
#     don2= list(2)
#     int = data[data[,time]==unique(data[,time])[j],]
#     for(i in 1:nrow(int)){
#       inter = list()
#       inter[[1]] = int[i,"hc-key"]
#       inter[[2]] = int[i,"value"]
#       names(inter) <- c("hc-key", "value")
#       don2[[i]] = inter
#     }
#     don[[j]] = don2
#   }
#   
#   res <- highchartsUtils::Highmaps$new()
#   
#   res$setScope(scope)
#   
#   res$mapNavigation(enabled = TRUE,
#                     buttonOptions = list(verticalAlign =  'bottom'))
#   
#   require(RJSONIO)
#   ldata = plyr::alply(don, 1, as.list)
#   
#   lrefdata= as.list(unique(data[,time]))
#   lrefdata=plyr::alply(lrefdata, 1, as.list)
#   
#   res$chartData(toJSON(ldata))
#   
#   res$RefchartData(toJSON(lrefdata))
#   
#   current.maps <- switch(scope,
#                          France_regions = "countries/fr/custom/fr-all-mainland",
#                          France_regions_dom = "countries/fr/fr-all",
#                          Europe = "custom/europe",
#                          UE = "custom/european-union")
#   
#   res$series(data = don[[1]] ,mapData = paste0("#! Highcharts.maps['", current.maps, "'] !#"),
#              joinBy ='hc-key',
#              name = serie.name,
#              states = list(  hover = list(color = '#BADA05')),
#              dataLabels = list(enabled = TRUE, format =  '{point.name}'))
#   
#   res$colorAxis(min = min(data[,"value"]), max = max(data[,"value"]), maxColor = "#0A0A2A")
#   
#   res$title(text = title)
#   
#   res$chart(height = 600)
#   
#   res$set(bodyattrs = "ng-app ng-controller='rChartsCtrl'")
#   
#   #res$addAssets(jshead = "https://ajax.googleapis.com/ajax/libs/angularjs/1.2.26/angular.min.js")
#   
#   range_time <- c(1,length(ldata))
#   res$setTemplate(chartDiv = sprintf("\n<div id='{{chartId}}' class='rChart datamaps'></div> 
#                                      <br></br>
#                                      <div class='container' style = 'text-align:center'>
#                                      <input id='slider' type='range' min=%s max=%s ng-model='time' ng-change='changeMap()' width=200>
#                                      <span ng-bind='currenttime'></span>
#                                      <button style='width:60px;height:30px' ng-click='animateMap()'>Play</button>
#                                      <button style='width:60px;height:30px' ng-click='pauseMap()' >Pause</button>
#                                      <button style='width:60px;height:30px' ng-click='resetMap()'>Stop</button>
#                                      <select ng-model='mySpeed' ng-options='speed.name for speed in speeds'></select><br>
#                                      
#                                      </div>      
#                                      <script>\
#                                      
#                                      var ctrl = 'play';
#                                      var play = 0;
#                                      function rChartsCtrl($scope, $timeout){
#                                      
#                                      $scope.speeds = [
# {name:'20/s', shade:50},
# {name:'10/s', shade:100},
# {name:'5/s', shade:250},
# {name:'2/s', shade:500},
# {name:'1/s', shade:1000},
# {name:'1/2s', shade:2000},
# {name:'1/5s', shade:5000},
# {name:'1/10s', shade:10000}
#                                      ];
#                                      
#                                      $scope.mySpeed = $scope.speeds[2];
#                                      
#                                      $scope.changespeed = 100;
#                                      $scope.time = %s;
#                                      $scope.currenttime = refdata[$scope.time]
#                                      
#                                      $scope.changeMap = function(){
#                                      ctrl = 'change'
#                                      var chart = $('#{{chartId}}').highcharts();
#                                      chart.series[0].update({data: data[$scope.time]});
#                                      $scope.currenttime = refdata[$scope.time]
#                                      
#                                      return;
#                                      }
#                                      
#                                      $scope.animateMap = function(){
#                                      if(ctrl == 'change'){  
#                                      ctrl = 'play'
#                                      $scope.time = $scope.time-1
#                                      }
#                                      if(ctrl == 'play'){
#                                      play = 1
#                                      if ($scope.time > %s){
#                                      ctrl = 'end'
#                                      play = 0
#                                      return;
#                                      }
#                                      $scope.currenttime = refdata[$scope.time]
#                                      var chart = $('#{{chartId}}').highcharts();
#                                      chart.series[0].update({data: data[$scope.time]});
#                                      $scope.time += 1;
#                                      $timeout($scope.animateMap, $scope.mySpeed.shade)
#                                      }else{
#                                      play = 0
#                                      ctrl = 'play'
#                                      return;
#                                      }
#                                      }
#                                      
#                                      $scope.pauseMap = function(){
#                                      if(ctrl == 'play' && play == 1){
#                                      ctrl = 'pause'
#                                      return;
#                                      }
#                                      ctrl = 'play'
#                                      return;
#                                      }
#                                      
#                                      $scope.resetMap = function(){
#                                      if(ctrl == 'end' || ctrl == 'play' & play == 0){
#                                      ctrl = 'play'
#                                      }else{
#                                      ctrl = 'reset'
#                                      }
#                                      $scope.time = %s
#                                      $scope.currenttime = refdata[$scope.time]
#                                      var chart = $('#{{chartId}}').highcharts();
#                                      chart.series[0].update({data: data[$scope.time]});
#                                      return;
#                                      }
#                                      
#                                      }\
#                                      </script>", 
#                                      range_time[1], range_time[2],range_time[1], range_time[2], range_time[1]))
# 
#   return(res)
# 
# }
