HapusAkhiran <- function(kata) {
  if ( grepl("[A-Za-z]+([kl]ah|pun|ku|mu|nya)$", kata) ) {
    kata1 <- sub("([klt]ah|pun|ku|mu|nya)$","",kata)
    if ( grepl("[A-Za-z]+([kl]ah|pun)$", kata) ) {
      kata1 <- HapusAkhiranKepunyaan(kata1)
    }
  } else {
    kata1 <- kata
  }
  kata1
}

HapusAkhiranKepunyaan <- function(kata) {
  if ( grepl("[A-Za-z]+(nya|[km]u)$", kata) ) {
    kata = sub("(nya|[km]u)$","",kata)
  }
  kata
}

HapusAkhiranIAnKan <- function(kata) {
  if ( grepl("(kan)$", kata) ) {
    kata1 <- sub("(an)$", "", kata)
    if ( is_katadasar(kata1) ) {
      kata <- kata1
    }
  }
  if ( grepl("(kan)$", kata) ) {
    kata2 <- sub("(kan)$", "", kata)
    if ( is_katadasar(kata2) ) {
      kata <- kata2
    }
  }

  if ( grepl("(i|[^k]an)$", kata) ) {
    kata3 <- sub("(i|an)$", "", kata)
    if ( is_katadasar(kata3) ) {
      kata <- kata3
    }
  }
  return(kata)
}
