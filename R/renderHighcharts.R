#' renderHighcharts (use with Shiny)
#' 
#' Use as Shiny output. First, use \code{renderHighcharts} in \code{server.R}
#' to assign the chart object to an Shiny output. Then create an highchartsOutput
#' with the same name in #' \code{ui.R}. \code{highchartsOutput} is currently just an
#' alias for \code{htmlOutput}.
#' 
#' @author Thomas Reinholdsson, Ramnath Vaidyanathan
#' @param expr An expression that returns a chart object
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is expr a quoted expression (with \code{quote()})? This is
#'  useful if you want to save an expression in a variable.
#'   
#' @export
#' 
#' @include utils.R
#' @include highchartsUtilsClass.R
renderHighcharts <- function(expr, env = parent.frame(), quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env, quoted)
  function() {
    highchartsUtils_ <- func()
    cht_style <- sprintf("<style>.highchartsUtils {width: %spx; height: %spx} </style>",
                         highchartsUtils_$params$width, highchartsUtils_$params$height)
    HTML(paste(c(cht_style, highchartsUtils_$html()), collapse = '\n'))
  }
}


#' renderChart2 (use with Shiny)
#' 
#' renderChart2 is a modified version of renderChart. While renderChart 
#' creates the chart directly on a shiny input div, renderChart2 uses the
#' shiny input div as a wrapper and appends a new chart div to it. This
#' has advantages in being able to keep chart creation workflow the same
#' across shiny and non-shiny applications
# renderChart2 <- function(expr, env = parent.frame(), quoted = FALSE) {
#   func <- shiny::exprToFunction(expr, env, quoted)
#   function() {
#     rChart_ <- func()
#     cht_style <- sprintf("<style>.rChart {width: %spx; height: %spx} </style>",
#       rChart_$params$width, rChart_$params$height)
#     cht <- paste(capture.output(rChart_$print()), collapse = '\n')
#     HTML(paste(c(cht_style, cht), collapse = '\n'))
#   }
# }
