\name{createMonteCarloGrid}
\alias{createMonteCarloGrid}
\title{
Create a multidimensional grid of nodes and weights for Monte Carlo integration
}
\description{
Simulate nodes using a random number generator supplied by the user, and combine these with a vector of equal weights into a list. Sparse grids can be created with the function createSparseGrid.
}
\usage{
createMonteCarloGrid( rng, dimension, num.sim, ... )
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rng}{
	function that generates random numbers. The first argument of this function should be called \code{n}. Examples are the R built-in functions \code{rnorm} and \code{runif} for random numbers from a standard normal or uniform distribution.
  }
  \item{dimension}{
	dimension of the integration problem.
  }
  \item{num.sim}{
	number of simulated integration nodes.
  }
  \item{...}{
	arguments that will be passed to the random number generator \code{rng}.
  }
}
\value{
    The return value contains a list with nodes and weights
    \item{nodes}{matrix with a node in each row}
    \item{weights}{vector with corresponding weights}
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

# set random seed
set.seed( 3141 )

# Create Monte Carlo integration grids
# 1. with draws from a uniform distribution
mc.grid <- createMonteCarloGrid( runif, dimension=2, num.sim=10 )
mc.grid

# 2. with draws from a standard normal distribution
mc.grid <- createMonteCarloGrid( rnorm, dimension=3, num.sim=1000 )

# 3. with draws from a normal distribution with mean=2 and sd=5
mc.grid <- createMonteCarloGrid( rnorm, dimension=3, num.sim=1000, mean=2, sd=5 )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ distribution }
\keyword{ multivariate }

