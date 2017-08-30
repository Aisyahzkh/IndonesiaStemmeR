#' @export stemKalimat
#'
#' @title
#' Word stemmer for Bahasa Indonesia
#'
#' @description
#' Provides function to stem (retrieve root word of a word) a word in
#' Bahasa Indonesia using Nazief and Andriani's algorithm.
#' It consists set of functions to remove prefixes, suffixes or both.
#' This package is made based on katadasaR package
#'
#' @author Aisyah Zakiah
#'
#' @param kalimat
#'
#' @examples stemKalimat("percobaan")
#'

stemKalimat <- function(kalimat) {
  kalimat <- as.character(kalimat)
  kalimat_split = strsplit(kalimat, split = " ")
  kalimat <- ""

  for(i in kalimat_split[[1]]){
    kata <- i
    kata1 <- kata
    katadasar <- FALSE;
    if (is_katadasar(kata)) {
      kalimat <- c(kalimat,kata)
    } else {
      kata <- HapusAkhiran(kata)
      if (!is.null(kata)) {
        if ( is_katadasar(kata) ) {
          kalimat <- c(kalimat,kata)
          katadasar<-TRUE
        }
      }
      if(!katadasar){
        kata0 <- HapusAkhiranIAnKan(kata)
        if ( !is.null(kata) ) {
          if ( is_katadasar(kata) ) {
            kalimat <- c(kalimat,kata)
            katadasar<-TRUE
          } else {

          }
        }
        if(!katadasar){
          kata0 <- HapusAwalan(kata)
          if ( !is.null(kata0) ) {
            if ( is_katadasar(kata0) ) {
              kalimat <- c(kalimat,kata0)
              katadasar<-TRUE
            }
          }

          if(!katadasar){
            kata0 <- HapusSisipan(kata)
            if ( !is.null(kata0) ) {
              if ( is_katadasar(kata0) ) {
                kalimat <- c(kalimat,kata0)
                katadasar<-TRUE
              }
            }

            if( !katadasar ){
              {kalimat <- c(kalimat,kata1)}
            }
          }
        }
      }
    }
  }
  return(paste(kalimat, collapse=" "))
}
