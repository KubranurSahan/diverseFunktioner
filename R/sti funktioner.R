# Funktionen bruges til at udtage mappenavn (delstring) fra en sti på en valgt niveau

#' Udvælg del af sti
#' @encoding UTF-8
#'
#' @description Funktionen bruges til at udtage mappenavn (delstring) fra en sti på en valgt niveau
#'
#' @param sti en character string, stien
#' @param niveau numeric/integer, niveauen på mappen, hvor 0 er den sidste/mindste undermappe
#'
#' @details
#'
#' @return mappenavn
#'
#' @seealso
#'
#' @export
#'
#' @examples udvaelDelAfSti(sti = "C:\\ Mappe niveau 2\\ Mappe niveau 1\\ Mappe niveau 0\\ filnavn.txt", niveau = 1) vil returnere "Mappe niveau 1"
udvaelgDelAfSti <- function(sti, niveau){

  if(!(class(niveau) == "numeric" | class(niveau) == "integer")){

    stop("Niveau skal være af type numeric eller integer.")

  }

  if(grepl("\\", sti, fixed = TRUE)){

  opdelerTegn <- "\\"

  } else {

    opdelerTegn <- "/"

  }

  if(niveau < 0 | niveau > (length(gregexpr(opdelerTegn, sti, fixed = TRUE)[[1]])-2)){

    stop(paste0("Den valgte niveau eksisterer ikke. Niveauet skal være mellem 0 og ", (length(gregexpr(opdelerTegn, sti, fixed = TRUE)[[1]])-2)))

  }else{

    substring(sti,
              gregexpr(opdelerTegn, sti, fixed = TRUE)[[1]][length(gregexpr(opdelerTegn, sti, fixed = TRUE)[[1]]) - (niveau+1)] + 1,
              gregexpr(opdelerTegn, sti, fixed = TRUE)[[1]][length(gregexpr(opdelerTegn, sti, fixed = TRUE)[[1]]) - niveau] - 1)
  }

}
