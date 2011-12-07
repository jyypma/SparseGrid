\name{createProductRuleGrid}
\alias{createProductRuleGrid}
\title{
Create a multidimensional grid of nodes and weights for integration
}
\description{
Creates nodes and weights according to the product rule, combining 1D nodes and weights. Sparse grids can be created with the function createSparseGrid.
}
\usage{
createProductRuleGrid(type, dimension, k, sym = FALSE)
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
    dimension of the integration problem.
}
  \item{k}{
    Accuracy level. The rule will be exact for polynomial up to total order 2k-1.
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
\code{\link[SparseGrid:createMonteCarloGrid]{createMonteCarloGrid}}
\code{\link[SparseGrid:createIntegrationGrid]{createIntegrationGrid}}
\code{\link{integrate}}
\code{\link[mvtnorm:pmvnorm]{pmvnorm}}
}
\examples{
# load library
library('SparseGrid')

# define function to be integrated
# g(x) = x[1] * x[2] * ... * x[n]
g <- function( x ) {
	return( prod( x ) )
}

#
# Create sparse integration grid to approximate integral of a function with uniform weights
#
sp.grid <- createSparseGrid( 'KPU', dimension=3, k=5 )

# number of nodes and weights
length( sp.grid$weights )

# evaluate function g in nodes
gx.sp <- apply( sp.grid$nodes, 1, g )

# take weighted sum to get approximation for the integral
val.sp <- gx.sp \%*\% sp.grid$weights

#
# Create integration grid to approximate integral of a function with uniform weights
#
pr.grid <- createProductRuleGrid( 'KPU', dimension=3, k=5 )

# number of nodes and weights
length( pr.grid$weights )

# evaluate function g in nodes
gx.pr <- apply( pr.grid$nodes, 1, g )

# take weighted sum to get approximation for the integral
val.pr <- gx.pr \%*\% pr.grid$weights

#
# Create integration grid to approximation integral using Monte Carlo simulation
#
set.seed( 3141 )
mc.grid <- createMonteCarloGrid( runif, dimension=3, num.sim=1000 )

# number of nodes and weights
length( mc.grid$weights )

# evaluate function g in MC nodes
gx.mc   <- apply( mc.grid$nodes, 1, g )

# take weighted sum to get approximation for the integral
# the weights are all equal to 1/1000 in this case
val.mc <- gx.mc \%*\% mc.grid$weights

val.sp
val.pr
val.mc
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ distribution }
\keyword{ multivariate }

