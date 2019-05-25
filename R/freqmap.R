#' @title Get Frequency Count of kmers
#'
#' @param dnastr - dna string
#' @param k - length of the k-mer
#'
#' @return A hash map of k-mers and their counts
#' @export
#'
#' @name freqmap
#'
#

library("pracma")
library('hash')

freqmap <- function(dnastr, k) {

    dnalen = nchar(dnastr)
    if (k > dnalen) {
      stop("k exceeds the length of dnastr")
    }
    dict = hash()
    for (i in 1:(dnalen-k+1)) {
      pattern = substr(dnastr,i,(i+k-1))
      if ( has.key(pattern,dict)) {
        dict[[pattern]] = dict[[pattern]] + 1
      } else {
        dict[[pattern]] = 1
      }
    }
    return(dict)
}
