#' Use as Shiny output. First, use \code{renderHighcharts} in \code{server.R} to assign 
#' the chart object to an Shiny output. Then create an chartOutput with the same name in #'
#' \code{ui.R}.
#' 
#' @author Thomas Reinholdsson, Ramnath Vaidyanathan
#' @param outputId output variable to read value from
#' @param lib name of js library used
#' @param package name where js library resides
#' @export
#' @include utils.R
#' @include highchartsUtilsClass.R
highchartsOutput <- highchartsOutput <- function(outputId, lib = "highcharts", package = 'highchartsUtils', 
    add_lib = TRUE){
  if (!is.null(lib)){
    LIB <- get_lib(lib, package = package)
  } else if (exists(".highchartsUtils_object")) {
    LIB <- .highchartsUtils_object$LIB
  }
  if (add_lib){
    suppressMessages(singleton(addResourcePath(LIB$name, LIB$url)))
  }
  div(
    id = outputId, 
    class=paste('shiny-html-output', 'highchartsUtils', basename(LIB$name)),
    ifelse(add_lib, tagList(get_assets_shiny(LIB)), "")
  )
}


get_assets_shiny <- function(LIB){
  assets <- get_assets(LIB, static = F)
  assets$jshead <- Filter(filter_jquery, assets$jshead)
  scripts <- lapply(assets$jshead, function(script){
    singleton(tags$head(tags$script(src = script, type = 'text/javascript')))
  })
  styles <- lapply(assets$css, function(style){
    singleton(tags$head(tags$link(href = style, rel="stylesheet")))
  })
  return(c(styles, scripts))
}

filter_jquery <- function(js){
  jquery = c('jquery.js', 'jquery.min.js', 'jquery-1.8.2.min.js', 'jquery-1.9.1.min.js')
  !(basename(js) %in% jquery)
}