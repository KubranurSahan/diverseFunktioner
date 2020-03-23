#' Tal med tusindtalsseparator
#' @encoding UTF-8
#'
#' @param x Et tal som man ønsker returneret med tusindtalsseparator
#'
#' @return X med tusindtal (,) og decimalseparator (.)
#' @export
#'
#' @examples talFormat(1000.10) vil returnere 1,000.10
talMedTusindtalssep <- function(x){

  format(x, big.mark = ",", decimal.mark = ".")

  }



#' Procent funktion
#' @encoding UTF-8
#'
#' @description Funktion til at beregne procent og returnere det med '%' tegn
#'
#' @param taeller et tal der repræsenterer tælleren
#' @param naevner et tal der repræsenterer nævneren
#' @param antalDecimal det ønskede antal decimaler i resultatet. Default er sat til to decimaler.
#'
#' @return procent med '%'-tegn
#' @export
#'
#' @examples procent(taeller = 25, naevner = 50, antalDecimal = 0) eller procent(25, 50, 0)
procent <- function(taeller, naevner, antalDecimal = 2){

  paste0(round((taeller / naevner) * 100, antalDecimal), "%")

}



#' Rund tallet til nærmeste Y
#' @encoding UTF-8
#'
#' @param x et tal man øsnker at runde til nærmeste Y
#' @param y et tal der definere det nærmeste tal-interval man vil runde op/ned til
#'
#' @return x rundet op/ned til den nærmeste multiplum af Y
#' @export
#'
#' @examples rundTilNaermesteY(21, 5)
rundTilNaermesteY <- function(x, y){

  y * round(x / y, 0)

}
