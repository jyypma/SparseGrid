\name{readASCGrid}
\alias{readASCGrid}
\title{
Read integration grid from file
}
\description{
This function reads nodes and weights with the format of the \code{.asc} files available from \url{http://www.sparse-grids.de}
}
\usage{
readASCGrid(filename, dimension)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{filename}{
        name of the file that you want to read. The extension should be included.
	}
	\item{dimension}{
		dimension of the grid that you want to read.
	}
}
\value{
    The return value contains a list with nodes and weights
    \item{nodes}{matrix with a node in each row}
    \item{weights}{vector with corresponding weights}
}
\references{
Florian Heiss, Viktor Winschel, Likelihood approximation by numerical integration on sparse grids, Journal of Econometrics, Volume 144, Issue 1, May 2008, Pages 62-80, \url{http://www.sparse-grids.de}
}
\author{
Jelmer Ypma
}
\seealso{
\code{\link[SparseGrid:createSparseGrid]{createSparseGrid}}
\code{\link[SparseGrid:createProductRuleGrid]{createProductRuleGrid}}
\code{\link[SparseGrid:createIntegrationGrid]{createIntegrationGrid}}
\code{\link{integrate}}
\code{\link[mvtnorm:pmvnorm]{pmvnorm}}
}
\examples{
# load library
library('SparseGrid')

\dontrun{
# read file (e.g. after downloading from www.sparse-grids.de)
ReadASCFile(filename='GQU_d3_l5.asc', dimension=3)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ distribution }
\keyword{ multivariate }

