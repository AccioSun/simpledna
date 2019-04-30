#' @title Most Frequent k-mer
#' @name mostfreq
#' @param dnastr - the dna string
#' @param k - The length of the k-mers to evaluate to find the most frequent k-mer
#'
#' @return A hash map showing the k-mer string(s) with the maximum frequency and the associated frequency
#' @export
#'
#'

library('hash')

mostfreq <- function(dnastr, k) {

    dnalen = nchar(dnastr)
    if (k > dnalen) {
      stop("k exceeds the length of dnastr")
    }
    freqs = freqmap(dnastr,k)
    mostfreqs = hash()
    maxval = max(values(freqs,USE.NAMES = FALSE))
    for (key in keys(freqs)) {
      if ( freqs[[key]] == maxval) {
        mostfreqs[[key]] = maxval
      }
    }
    return(mostfreqs)
}
