% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sti funktioner.R
\encoding{UTF-8}
\name{udvaelgDelAfSti}
\alias{udvaelgDelAfSti}
\title{Udvælg del af sti}
\usage{
udvaelgDelAfSti(sti, niveau)
}
\arguments{
\item{sti}{en character string, stien}

\item{niveau}{numeric/integer, niveauen på mappen, hvor 0 er den sidste/mindste undermappe}
}
\value{
mappenavn
}
\description{
Funktionen bruges til at udtage mappenavn (delstring) fra en sti på en valgt niveau
}
\details{

}
\examples{
udvaelDelAfSti(sti = "C:\\\\ Mappe niveau 2\\\\ Mappe niveau 1\\\\ Mappe niveau 0\\\\ filnavn.txt", niveau = 1) vil returnere "Mappe niveau 1"
}
\seealso{

}
