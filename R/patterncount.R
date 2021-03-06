#' @title Count of the Given Pattern
#' @name patterncount
#' @param dnastr - a string containing the dna sequence to be analyzed
#' @param pattern - pattern to look for
#'
#' @return a number showing the count of the occurrences of the pattern
#' @export
#'
#' @examples
#'
#'
#'

library(pracma)
patterncount <- function(dnastr, pattern) {
    kount = 0
    dnalen = nchar(dnastr)
    patlen = nchar(pattern)
    if (patlen > dnalen) {
      stop("pattern is longer than dnastr")
    }
    for (i in 1:(dnalen-patlen+1)) {
      if ( strcmp( pattern, substr(dnastr,i, i+patlen-1))){
         kount = kount + 1
       }
    }
    return(kount )
}
