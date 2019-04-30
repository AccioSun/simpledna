#' @title Hamming Distance
#' @name hamming
#' @param pattern1 - pattern1 to compare with pattern2
#' @param pattern2 - pattern2 of equal length with pattern1
#'
#' @return a number showing the number of differences between the patterns
#' @export
#'
#' @examples
#'

library(pracma)
hamming <- function(pattern1, pattern2) {
    kount = 0
    patlen1 = nchar(pattern1)
    patlen2 = nchar(pattern2)
    if (patlen1 != patlen2) {
      stop("patterns are of unequal length")
    }
    for (i in 1:patlen1) {
      if ( !strcmp( substr(pattern1,i,i), substr(pattern2,i,i) )){
         kount = kount + 1
       }
    }
    return(kount )
}
