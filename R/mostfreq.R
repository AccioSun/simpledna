
#' Most Frequent k-mer
#'
#' @param dnastr
#' @param k
#'
#' @return
#' @export
#'
#' @examples
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
