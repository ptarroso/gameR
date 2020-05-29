\name{envrules}
\alias{envrules}
\title{
	Environmental rules.
}
\description{
    The extra environmental rules for the Game of Life.
}
\usage{
envrules(lraster, env, mean=0, sd=1, alpha=0.05)
}
\arguments{
  \item{lraster}{
    A raster where living and dead cells are processed.
	}
	\item{env}{
    A raster object with exactly the same proprieties as lraster but with values
		defining the environmental setting. For instance, a climate raster.
	}
	\item{mean}{
    The mean value of a normal distribution defining the suitable values in
		relation to the env raster.
	}
	\item{sd}{
		The standard deviation of a normal distribution defining the suitable values
		in relation to the env raster.
	}
	\item{alpha}{
    Suitable values in the normal distribution are found within the interval
		(alpha, 1-alpha).
	}
}
\details{
	This function accepts a raster with ones and zeros (alive and dead cells) and
	a second raster with same extent and resolution that has environmental
	information. Based on a normal distribution with 'mean' and 'sd', cells that
	are alive but on unsuitable areas are set to zero.

}
\value{
	A raster representing the alive/dead cells in the following generation.
}
\author{
	Pedro Tarroso
}
\examples{
	# not a particularly good example...
	require(raster)
	map <- raster(ncol=360, nrow=180)
	gen0 <- prepareGrid(map)
	env <- gen0*0 + rnorm(cells(gen0))
	gen1 <- liferules(lraster)
	gen1b <- envrules(gen1, env, mean=0, sd=0.5)
	plot(stack(gen0, env, gen1, gen1b))

}
