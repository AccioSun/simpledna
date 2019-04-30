#' @title The GC Content
#' @name GCcontent
#' @param dnastr - the dna string
#'
#' @return A fraction representing the (G+C) portion of the dnastr
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
