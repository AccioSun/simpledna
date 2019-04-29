
#' Title
#'
#' @param dnastr
#' @param pattern
#'
#' @return
#' @export
#'
#' @examples
#'

library(pracma)
patternpositions <- function(dnastr, pattern) {
    posns = vector()
    dnalen = nchar(dnastr)
    patlen = nchar(pattern)
    if (patlen > dnalen) {
      stop("pattern is longer than dnastr")
    }
    for (i in 1:(dnalen-patlen+1)) {
      if ( strcmp( pattern, substr(dnastr,i, i+patlen-1))){
         posns = c(posns, i)
       }
    }
    return(posns)
}
