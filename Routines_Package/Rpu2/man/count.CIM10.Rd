% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rapport.R
\name{count.CIM10}
\alias{count.CIM10}
\title{Combien de codes CIM10}
\usage{
count.CIM10(vx)
}
\arguments{
\item{vx}{un vecteur de character}
}
\value{
n nombre de codes CIM1
}
\description{
examine un vecteur de caractères et compte le nombre de mots compatibles avec un code CIM10
NA n'est pas compté comme un code CIM10
}
\examples{
count.CIM10(dx[dx$FINESS == "Col", "MOTIF"])
 @export

}
\author{
JcB
}

