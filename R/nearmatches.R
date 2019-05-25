#' @title Near Pattern Match Count
#' @name nearmatches
#' @param pattern  - pattern to search for
#' @param dnastr -   dna string to search the pattern in
#' @param d - Number of differences away the closest match can be
#'
#' @return a number showing the number of near matches for the pattern with d deviations
#' @export
#'
#' @examples
#'

nearmatches <- function(pattern, dnastr, d) {
    kount = 0
    dnalen = nchar(dnastr)
    patlen = nchar(pattern)
    if (patlen > dnalen) {
      stop("pattern is longer than dnastr")
    }
    for (i in 1:(dnalen-patlen+1)) {
      pattern2 = substr(dnastr,i, i+patlen-1)
      hamd = hamming(pattern, pattern2)
      if ( hamd <= d){
        kount = kount + 1
      }
    }
    return(kount )
}
