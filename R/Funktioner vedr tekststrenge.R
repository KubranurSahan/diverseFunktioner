# Funktionen bruges til at rette en tekst som er opdelt med bindestreger om til lowerCamelCase
#' Ret tekst med bindestreg opdeling til lowerCamelCase
#' @encoding UTF-8
#'
#' @param tekst en chracter string, teksten med bindestreg-opdeling
#'
#' @return 'tekst' i lowerCamelCase form
#' @export
#'
# @examples x
retBindestregTekstTilLCC<-function(tekst){

  # Finder placering af første tegn der ikke er en bindestreg
  startVedPlacering <- min(unlist(gregexpr("[^_ ]", tekst)))

  # Finder placeringerne for alle bindestreger i teksten
  bindestreger <- unlist(gregexpr("[_ ]", tekst))

  # Udvælgelger de bindestreger der ligger efter første tegn som ikke er en bindestreg
  bindestreger <- bindestreger[bindestreger > startVedPlacering]

  # Beregner afstanden mellem bindestregerne
  bindestregerAfstand <- c(bindestreger[-1], 10000) - bindestreger

  # Returnerer samme input, hvis tekst ikke indeholder bindestreger
  if(min(bindestreger) == -1){

    # Returnere teksten ved at lave første bogstav om til lowercase, hvis ikke der findes bindestreger eller mellemrum i teksten
    tekstReturnA <- paste0(tolower(stri_sub(tekst, startVedPlacering, startVedPlacering)),
                           stri_sub(tekst, startVedPlacering + 1, - 1))

  } else {

    # Starter med at lave første del af teksten til lowercase
    tekstReturnA <- paste0(tolower(stri_sub(tekst, startVedPlacering, startVedPlacering)),
                           stri_sub(tekst, startVedPlacering + 1, min(bindestreger) - 1))

    # Ændrer resten af teksten til sådan så første tegn efter en "_" er af uppercase
    for(i in 1:length(bindestreger)){

      if(bindestregerAfstand[i] == 1){

      } else {

        if(i != length((bindestreger))){

          tekstReturnA <- paste0(tekstReturnA,
                                 toupper(stri_sub(tekst, bindestreger[i] + 1, bindestreger[i] + 1)),
                                 stri_sub(tekst, bindestreger[i] + 2, bindestreger[i+1] - 1))

        } else {

          tekstReturnA <- paste0(tekstReturnA,
                                 toupper(stri_sub(tekst, bindestreger[i] + 1, bindestreger[i] + 1)),
                                 stri_sub(tekst, bindestreger[i] + 2, -1))

        }

      }

    }

  }

  return(tekstReturnA)

}



# Funktionen bruges til at rette en tekst opdelt med andet tegn end bogstaver og tal til lowerCamelCase
#' Ret tekst til lowerCamelCase
#' @encoding UTF-8
#'
#' @param tekst en character string, teksten
#'
#' @return 'tekst' i lowerCamelCase form
#' @export
#'
# @examples x
retTekstTilLCC<-function(tekst){

  # Finder placering af første tegn alphanumeric tegn i teksten
  startVedPlacering <- min(unlist(gregexpr("[a-zA-Z0-9]", tekst)))

  # Finder placeringerne for alle ikke alphanumeric tegn i teksten
  placeringer <- unlist(gregexpr("[^a-zA-Z0-9]", tekst))

  # Udvælgelger de tegn der ligger efter den første alphanumric tegn
  placeringer <- placeringer[placeringer > startVedPlacering]

  # Beregner afstanden mellem placeringerne
  placeringerAfstand <- c(placeringer[-1], 10000) - placeringer

  # Meldr fejl, hvis tekst ikke indeholder alphanumeric tegn
  if(startVedPlacering == -1){

    stop("Teksten indeholder ingen alphanumeric tegn. Teksten kan hermed ikke laves om til en lower camel case.")

  } else if(length(placeringer) == 0){

    # Returnere teksten ved at lave første bogstav om til lowercase, hvis ikke der findes andet end alphanumeric tegn i teksten
    tekstReturnA <- paste0(tolower(stri_sub(tekst, startVedPlacering, startVedPlacering)),
                           stri_sub(tekst, startVedPlacering + 1, - 1))

  } else {

    # Starter med at lave første del af teksten til lowercase
    tekstReturnA <- paste0(tolower(stri_sub(tekst, startVedPlacering, startVedPlacering)),
                           stri_sub(tekst, startVedPlacering + 1, min(placeringer) - 1))

    # Ændrer resten af teksten til sådan så første tegn efter en "_" er af uppercase
    for(i in 1:length(placeringer)){

      if(placeringerAfstand[i] == 1){

      } else {

        if(i != length((placeringer))){

          tekstReturnA <- paste0(tekstReturnA,
                                 toupper(stri_sub(tekst, placeringer[i] + 1, placeringer[i] + 1)),
                                 stri_sub(tekst, placeringer[i] + 2, placeringer[i+1] - 1))

        } else {

          tekstReturnA <- paste0(tekstReturnA,
                                 toupper(stri_sub(tekst, placeringer[i] + 1, placeringer[i] + 1)),
                                 stri_sub(tekst, placeringer[i] + 2, -1))

        }

      }

    }

  }

  return(tekstReturnA)

}



#' Opdel (lower) Camel Case tekst til tekst med mellemrum
#' @encoding UTF-8
#'
#' @param tekst en tekst af typen lowerCamelCase eller UpperCamelCase.
#'
#' @return 'tekst' opdelt med mellemrum
#' @export
#'
#' @examples opdelCamelCaseTekst("lowerCamelCase")
opdelCamelCaseTekst <- function(tekst){

  trimws(tolower(gsub("([A-Z])", " \\1", tekst)))

}


#' Ændre første bogstav i tekst til stort bogstav
#' @encoding UTF-8
#'
#' @param x en tekststreng
#'
#' @return teksten x med stort begyndelsesbogstav
#' @export
#'
#' @examples foersteBogstavTilStor("hej")
foersteBogstavTilStor <- function(x){

  paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))

}


#' Ændre første bogstav i tekst til lille bogstav
#' @encoding UTF-8
#'
#' @param x en tekststreng
#'
#' @return teksten x med lille begyndelsesbogstav
#' @export
#'
#' @examples foersteBogstavTilLille("Hej")
foersteBogstavTilLille <- function(x){

  paste0(tolower(substring(x, 1, 1)), substring(x, 2, nchar(x)))

}
