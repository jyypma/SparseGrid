\name{createIntegrationGrid}
\alias{createIntegrationGrid}
\title{
Create integration grid with the least number of nodes, either using a sparse grid or a product rule grid.
}
\description{
This function creates nodes and weights that can be used for integration. It is a convenience function that calls createSparseGrid and createProductRuleGrid and returns the grid with the least number of nodes. Typically, a grid created by the product rule will only contain fewer nodes than a sparse grid for very low dimensions.
}
\usage{
createIntegrationGrid(type, dimension, k, sym = FALSE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{
    String or function for type of 1D integration rule, can take on values
	\describe{
        \item{\code{"KPU"}}{Nested rule for unweighted integral over [0,1]}
        \item{\code{"KPN"}}{Nested rule for integral with Gaussian weight}
        \item{\code{"GQU"}}{Gaussian quadrature for unweighted integral over [0,1] (Gauss-Legendre)}
        \item{\code{"GQN"}}{Gaussian quadrature for integral with Gaussian weight (Gauss-Hermite)}
        \item{\code{func}}{any function. Function must accept level k and return a list with two elements \code{nodes} and \code{weights} for univariate quadrature rule with polynomial exactness 2k-1.}
	}
}
  \item{dimension}{
    Dimension of the integration problem.
}
  \item{k}{
    Accuracy level. The rule will be exact for polynomials up to total order 2k-1.
}
  \item{sym}{
    (optional) only used for own 1D quadrature rule (type not "KPU",...). If
            sym is supplied and not FALSE, the code will run faster but will
            produce incorrect results if 1D quadrature rule is asymmetric.
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
\code{\link[SparseGrid:createMonteCarloGrid]{createMonteCarloGrid}}
\code{\link{integrate}}
\code{\link[mvtnorm:pmvnorm]{pmvnorm}}
}
\examples{
# load library
library('SparseGrid')

# create integration grid
int.grid <- createIntegrationGrid( 'GQU', dimension=3, k=5 )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ distribution }
\keyword{ multivariate }

