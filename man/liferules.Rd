\name{liferules}
\alias{liferules}
\title{
	The Game of Life rules.
}
\description{
    The set of rules for the Game of Life.
}
\usage{
liferules(lraster, nearNA=F)
}
\arguments{
  \item{lraster}{
    A raster where living and dead cells are processed.
	}
  \item{nearNA}{
    If TRUE, then alive cells near NSs (ocean) live forever.
	}
}
\details{
	This function accepts a raster with ones and zeros (alive and dead cells) and
	processes each living cell accordingly to the original rules. However, since
	the raster is not and infinite plane, living cells at the edge of the raster
	are eliminated. If a cell has an NA neighbor, and if nearNA is set to TRUE,
	then that cell lives forever.

}
\value{
	A raster representing the alive/dead cells in the following generation.
}
\author{
	Pedro Tarroso
}
\examples{
	require(raster)
	map <- raster(ncol=360, nrow=180)
	gen0 <- prepareGrid(map)
	gen1 <- liferules(lraster)
	plot(stack(gen0, gen1))

}
