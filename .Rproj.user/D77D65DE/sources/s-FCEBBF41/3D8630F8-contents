
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
#'

library('hash')
GCcontent <- function(dnastr) {
  dnalen = nchar(dnastr)
  atgcmap = freqmap(dnastr,1)
  return ( (atgcmap[['G']] + atgcmap[['C']]) / dnalen)
}
