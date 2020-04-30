source('HybridPackage.R')

test_lz <- "ababcbababaa"
lz_res1 <- lz77_compressor(test_lz, as.vec=F)
lz77_decompressor(lz_res1)

test_lz2 <- "abacabacabadaca"
lz_res2 <- lz77_compressor(test_lz2, as.vec=T)
lz77_decompressor(lz_res2)

test_lz3 <- "abacabadabacabae"
lz_res3 <- lz77_compressor(test_lz3)
lz77_decompressor(lz_res3)

test_lz4 <- "kabababababz"
lz_res4 <- lz77_compressor(test_lz4)
lz77_decompressor(lz_res4)

test_rle <- c("aaadf1111111111")

res_rle1 <- RLE_compressor(test_rle, as.vec=F)
RLE_decompressor(res_rle1)

res_rle2 <- RLE_compressor(test_rle, as.bytes=T)
RLE_decompressor(res_rle2, as.bytes=T)

res_rle3 <- RLE_compressor(test_rle, to.raw = T)
RLE_decompressor(res_rle3, from.raw=T)

res_rle4 <- RLE_compressor(test_rle, as.vec = F)
RLE_decompressor(res_rle4)

test_rle2 <- "AAAAAAbbbXXXXXt"
res_rle5 <- RLE_compressor(test_rle2)
RLE_decompressor(res_rle5, as.vec=F)

hybrid_res <- Hybrid_compressor(test_rle2)
hybrid_res

hybrid_res2 <- Hybrid_decompressor(hybrid_res)
hybrid_res2 #output:AAAAAAbbbXXXXXt(test_rle2)
