#' test-simpledna
#'
#' This is a test script file for testing the functions of the simpledna R package.
#'
#'
#' It uses a test dataset den-1.fasta.txt which is the complete genome of Dengue virus 1
#' in fasta format
#' @param  None
#' @return None
#' @export

testsimpledna <- function() {

  # Read the file into a string
  fastaarr <- readLines("testdata/den-1.fasta.txt")
  dnaarr = fastaarr[2:length(fastaarr)]
  dnastr = paste(dnaarr, collapse="")

  # patterncount
  patterncount(dnastr,"GCTG")
  patterncount(dnastr,"ATTATC")

  # patternpositions
  patternpositions(dnastr,"ATTATC")

  # freqmap - for a given length of k, frequency of all unique "k-mers".  A k-mer is a contiguous string of length k
  # anywhere in the dnastr
  freqmap(dnastr,4)
  freqmap(dnastr,3)
  freqmap(dnastr,1)

  # GCContent
  GCcontent(dnastr)

  # mostfreq - for a given length of k, what is the most frequent k-mer ?
  mostfreq(dnastr,3)
  mostfreq(dnastr,4)

  # Hamming Distance
  hamming("ADTCD", "ADTDD")
  hamming("AGCTD", "ACGTA")

  # Near Matches
  nearmatches("ATCACC", dnastr,2)

}
