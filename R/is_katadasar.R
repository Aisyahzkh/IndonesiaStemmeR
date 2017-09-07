is_katadasar <- function(kata, kamus=NULL) {
  if ( is.null(kamus) ) {
    kata_dasar <- unique(kamus_katadasar)
  } else if ( is.vector(kamus) ) {
    kata_dasar <- unique(c(kamus_katadasar, kamus))
  } else {
    stop("Kamus harus berupa vektor!")
  }

  b <- !is.na(match(kata, kata_dasar))
  return(b)
}
