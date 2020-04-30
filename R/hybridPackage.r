require(stringi)

#' RLE compressor. Encodes single-element character vector or vector of characters
#'
#' @param s character of vector of characters
#' @param as.bytes Logical value, default FALSE. If T - the counts of characters are
#'                 converted to bytes, if F - are left as they are. Is useful for
#'                 text with digits or with long substrings (e.g 'aaa123aaa' or 'asddddddddddda')
#' @param to.raw Logical value, default FALSE. If T - returns raw-data, else - characters vector
#' @param as.vec Logical value, default TRUE. If T - returns vector of characters,
#'               else - one-element character vector
#' @return Raw-data or vector of characters of one-element character
#' @export
#' @examples
#' test_rle <- c("aaadf1111111111")
#'  RLE_compressor(test_rle, as.vec=F)
#'  RLE_compressor(test_rle, as.bytes=T)
#'   RLE_compressor(test_rle, to.raw = T)
RLE_compressor <- function(s, as.bytes=F, to.raw=F, as.vec = T) {
  block_size <- 256
  s <- paste(s, collapse='')

  res <- c()
  cur_symbol <- substr(s, 1, 1)
  cur_count <- 1

  add_number <- function(n) {
    if (as.bytes)
      return(intToUtf8(n))
    else
      return(n)
  }
  convert_result <- function(res) {
    if (to.raw)
      return(charToRaw(paste(res, collapse='')))
    if (as.vec)
      return(res)
    else
      return(paste(res, collapse=''))
  }


  for (i in 2:stri_length(s)) {
    if (substr(s, i, i) == cur_symbol) {
      if (cur_count == block_size - 1) {
        res <- c(res, add_number(cur_count), cur_symbol)
        cur_count <- 0
      }
      cur_count <- cur_count + 1
    }
    else {
      res <- c(res, add_number(cur_count), cur_symbol)
      cur_count <- 1
      cur_symbol <- substr(s, i, i)
    }
  }
  res <- c(res, add_number(cur_count), cur_symbol)

  convert_result(res)
}

#' RLE decoder. Decodes output from the encoder
#'
#' @param s output from the encoder (raw, character or vector)
#' @param as.bytes Logical value, default FALSE. If T - the counts of characters are
#'                 converted from bytes, if F - are left as they are. Should be the same
#'                 as for the encoder
#' @param from.raw Logical value, default FALSE. If T - decodes raw-data, else - characters vector
#'                 Should be the same as for the encoder
#' @param as.vec Logical value, default TRUE. If T - returns vector of characters,
#'               else - one-element character vector
#' @return Vector of characters of one-element character
#' @export
#' @examples
#' test_rle <- c("aaadf1111111111")
#'  RLE_decompressor(RLE_compressor(test_rle, as.bytes=T), as.bytes=T)
#'  RLE_decompressor(RLE_compressor(test_rle, to.raw = T), from.raw=T)
RLE_decompressor <- function(s, as.bytes=F, from.raw=F, as.vec=T) {
  split_single <- function(s) {
    res_s <- c()
    m_symb <- c()
    symb <- c()
    for (i in 1:stri_length(s)) {
      symb <- substr(s, i, i)
      if (grepl('[0-9]', symb)) {
        m_symb <- c(m_symb, symb)
        next
      }
      else {
        if (length(m_symb)) {
          res_s <- c(res_s, paste(m_symb, collapse=''))
          m_symb <- c()
        }
      }
      res_s <- c(res_s, symb)
    }
    if (length(m_symb)) {
      if (grepl('[0-9]', symb)) {
        m_symb <- m_symb[-length(m_symb)]
      }
      if (length(m_symb))
        res_s <- c(res_s, paste(m_symb, collapse=''))
      res_s <- c(res_s, symb)
    }
    return(res_s)
  }

  res = s
  if (from.raw) {
    res <- rawToChar(s)
  }
  if (length(res) < 2) {
    res <- split_single(res)
  }

  lens <- res[seq(1,length(res), 2)]
  if (as.bytes) {
    lens <- sapply(lens, utf8ToInt)
  }
  symbols <- res[seq(2,length(res), 2)]

  if (as.vec)
    return(rep(symbols,lens))
  else
    return(paste(rep(symbols,lens), collapse=''))
}

#' LZ77 compressor. Encodes single-element character vector or vector of characters
#'
#' @param s character of vector of characters
#' @param as.vec Logical value, default FALSE. If T - returns vector of characters,
#'               else - list of triplets (offset, length, symbol)
#' @return List of triplets or vector of characters
#' @export
#' @examples
#' test_lz <- "abacabacabadaca"
#'  lz_res <- lz77_compressor(test_lz, as.vec=T)
lz77_compressor <- function(s, as.vec = F) {
  s <- paste(s, collapse='')
  is.previous <- function(symb, pos) {
    l <- stri_length(symb)
    res <- c(0,0)
    if (pos == 1 || l >= pos) {
      return(res)
    }
    for (k in (pos-1):l) {
      start <- k - l + 1
      if (substr(s, start, k) == symb) {
        res <- c(pos - start, l)
        break
      }
    }
    return(res)
  }

  res <- list()
  offset <- 0
  for (i in 1:stri_length(s)) {
    if (offset > 0) {
      offset <- offset - 1
      next
    }
    symbol_acc <- substr(s, i, i)

    s_found <- c(0,0)
    s_found_prev <- c(0,0)
    repeat {
      s_found <- is.previous(symbol_acc, i)
      if (i + offset >= stri_length(s) || (s_found[1] == 0 && s_found[2] == 0)) {
        break
      }
      s_found_prev <- s_found
      offset <- offset + 1
      symbol_acc <- paste(substr(s, i, i + offset), collapse='')
    }
    symbol_acc <- paste(symbol_acc, collapse='')
    res <- c(res, list(c(s_found_prev[1], s_found_prev[2],
                         substr(s, i + offset, i + offset))), recursive=F)
  }

  if (as.vec)
    return(unlist(res))
  else
    return(res)
}

#' LZ77 decompressor. Encodes output from encoder (either list of triplet or vector)
#'
#' @param l_s list of triplets (offset, length, symbol) or vector of characters
#' @return vector of characters
#' @export
#' @examples
#' test_lz <- "abacabacabadaca"
#'  lz_res <- lz77_decompressor(encode_lz77(test_lz, as.vec=T))
lz77_decompressor <- function(l_s) {
  l_s <- unlist(l_s)
  offsets <- as.integer(l_s[seq(1, length(l_s), 3)])
  lenghts <- as.integer(l_s[seq(2, length(l_s), 3)])
  letters <- l_s[seq(3, length(l_s), 3)]
  s_res <- c()
  for (i in 1:length(letters)) {
    if (offsets[i] == 0 && lenghts[i] == 0) {
      s_res <- paste(c(s_res, letters[i]), collapse='')
    }
    else {
      l <- stri_length(s_res)
      start <- l - offsets[i] + 1
      end <- start + lenghts[i] - 1
      s_res <- paste(c(s_res, substr(s_res, start, end), letters[i]), collapse='')
    }
  }
  return(s_res)
}

#' Hybrid compressor. Encodes the string with RLE compressor followed by lz77 compressor (either list of triplet or vector)
#'
#' @param l_s list of triplets (offset, length, symbol) or vector of characters
#' @return vector of characters
#' @export
#' @examples
#' test_lz <- "abacabacabadaca"
#'  hybrid_res <- lz77_compressor(RLE_compressor(test_lz, as.vec=T))
Hybrid_compressor <- function(s) {
  lz77_compressor(RLE_compressor(s, as.vec = T), as.vec=T)
}

#' Hybrid decompressor. decodes the string with lz77 decompressor followed by RLE compressor (either list of triplet or vector)
#'
#' @param l_s list of triplets (offset, length, symbol) or vector of characters
#' @return vector of characters
#' @export
#' @examples
#' test_lz <- "abacabacabadaca"
#'  hybrid_res <- RLE_decompressor(lz77_decompressor(test_lz, as.vec=T))
Hybrid_decompressor <- function(s) {
  RLE_decompressor(lz77_decompressor(s), as.vec = F)
}
