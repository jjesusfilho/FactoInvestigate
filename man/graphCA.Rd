\name{graphCA}
\alias{graphCA}

\title{
Correspondance Analysis factor map
}
\description{
Realise the Correspondance Analysis overlayed graph
}

\usage{
graphCA(res, file = "", dim = 1:2, Rselec = "cos2", Cselec = "cos2", Rcoef = 1, 
        Ccoef = 1, figure.title = "Figure", graph = TRUE, cex = 0.7)
}

\arguments{
  \item{res}{
an object of class CA.
}
  \item{file}{
the file path where to write the description in Rmarkdown langage. If not specified, the description is written in the console.
}
  \item{dim}{
a 2 dimensional numerical vector giving the factorial dimensions to use for the representation (by default the first plane)
}
  \item{Rselec}{
the rows to select ; see the details section.
}
  \item{Cselec}{
the columns to select ; see the details section.
}
  \item{Rcoef}{
a numerical coefficient to adjust the rows selection rule ; see the details section.
}
  \item{Ccoef}{
a numerical coefficient to adjust the columns selection rule ; see the details section.
}
  \item{figure.title}{
the text label to add before graph title.
}
  \item{graph}{
a boolean : if \code{TRUE}, graphs are ploted.
}
  \item{cex}{
an optional argument for the generic plot functions, used to adjust the size of the elements ploted.
}
}

\details{
The \code{Rselec} argument (respectively \code{Cselec}) is used in order to select a part of the elements that are drawn and described. For example, you can use either :
\cr - \code{Rselec = 1:5} then the rows (the columns) numbered 1 to 5 are drawn.
\cr - \code{Rselec = c("name1","name5")} and then the rows (the columns)  named \code{name1} and \code{name5} are drawn.
\cr - \code{Rselec = "contrib 10"} then the 10 active or illustrative rows (the columns)  that have the highest contribution on the 2 dimensions of the plane are drawn.
\cr - \code{Rselec = "contrib"} then the optimal number of active or illustrative rows (the columns)  that have the highest contribution on the 2 dimensions of the plane are drawn.
\cr - \code{Rselec = "cos2 5"} then the 5 active or illustrative rows (the columns)  that have the highest cos2 on the 2 dimensions of the plane are drawn.
\cr - \code{Rselec = "cos2 0.8"} then the active or illustrative rows (the columns)  that have a \code{cos2} higher to \code{0.8} on the plane are drawn.
\cr - \code{Rselec = "cos2"} then the optimal number of active or illustrative rows (the columns)  that have the highest cos2 on the 2 dimensions of the plane are drawn.
\cr 
\cr The \code{Rcoef} argument (respectively \code{Ccoef}) is used in order to adjust the selection of the elements when based on \code{Rselec = "contrib"} or \code{Rselec = "cos2"}. For example :
\cr - if \code{Rcoef = 2}, the threshold is 2 times higher, and thus 2 times more restrictive.
\cr - if \code{Rcoef = 0.5}, the threshold is 2 times lower, and thus 2 times less restrictive.
}

\author{
Simon THULEAU, with the participation of Fran�ois HUSSON
}

\seealso{
  \code{\link{factoGraph}}, \code{\link{graphInd}}, \code{\link{graphHab}}, \code{\link{graphVar}}, \code{\link{graphSup}}
}

\examples{
require(FactoMineR)
data(children)
res.ca = CA(children, row.sup = 15:18, col.sup = 6:8, graph = FALSE)
graphCA(res.ca)
}

\keyword{graphs}