HapusSisipan <- function(kata){
    if ( grepl(".+(er).+", kata) ) {
      kata1 <- HapusSisipanEr(kata)
      if ( !is.null(kata1) ) {
        return(kata1)
      }
    }
    if ( grepl(".+(el).+", kata) ) {
      kata1 <- HapusSisipanEl(kata)
      if ( !is.null(kata1) ) {
        return(kata1)
      }
    }
    if ( grepl(".+(em).+", kata) ){
      kata1 <- HapusSisipanEm(kata)
      if ( !is.null(kata1) ) {
        return(kata1)
      }
  }
}

HapusSisipanEr <- function(kata) {
  kata1 <- sub("(er)", "", kata)
  if ( is_katadasar(kata1) ) {
    return(kata1)
  }
}
HapusSisipanEl <- function(kata) {
  kata1 <- sub("(el)", "", kata)
  if ( is_katadasar(kata1) ) {
    return(kata1)
  }
}
HapusSisipanEm <- function(kata) {
  kata1 <- sub("(em)", "", kata)
  if ( is_katadasar(kata1) ) {
    return(kata1)
  }
}
