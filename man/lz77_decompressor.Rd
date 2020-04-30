\name{lz77_decompressor}
\alias{lz77_decompressor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{LZ77 decoder. Eecodes output from encoder (either list of triplet or vector)}
\description{
LZ77 decoder. Eecodes output from encoder (either list of triplet or vector
}
\usage{
lz77_decompressor(l_s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{l_s}{
list of triplets (offset, length, symbol) or vector of characters
}
}

\value{
vector of characters
}

\examples{
test_lz <- "abacabacabadaca"
 lz_res <- decode_lz77(encode_lz77(test_lz, as.vec=T))

}
