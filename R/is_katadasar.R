is_katadasar <- function(kata) {
  kata_dasar <- unique(kamus_katadasar)
  b <- !is.na(match(kata, kata_dasar))
  return(b)
}
