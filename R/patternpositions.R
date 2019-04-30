#' @title Positions of the Pattern in dna String
#' @name patternpositions
#' @param dnastr - a string containing the dna sequence to be analyzed
#' @param pattern - pattern to look for
#'
#' @return a vector of the positions of the occurrences of the pattern in the dnastr
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
