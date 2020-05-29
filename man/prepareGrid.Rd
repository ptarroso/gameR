\name{prepareGrid}
\alias{prepareGrid}
\title{
	Prepare a grid with patterns
}
\description{
    Prepare a grid with patterns to play the Game of Life.
}
\usage{
prepareGrid(map, elem=list(), maxiter=1000, n=20)
}
\arguments{
  \item{map}{
	  A raster map that is used as a model to prepare the grid with the alive/dead
		cells.
	}
	\item{elem}{
    A list of elements to add to the map. See details on how to use it.
	}
	\item{maxiter}{
    The maximum number of attempts to place elements in map resolving conflits.
    With small maps it is easily reached. Solution: increase the number, 
    increase the available area, or decrease the number of elements to be placed.
	}
	\item{n}{
    Number of random elements to *try* to place in the grid if elem is an empty
    list. Defaults to 20. An element is only placed if maxiter is not reached.
	}
}
\details{
	It prepares a grid with cells that are alive (value of one) on a background of
	zeros. It copies the NAs distribution from map.
	Elements can be added randomly or manually. If 'elem' is an empty list
	(default), than a random number of patterns is placed within the data area
	(non NA pixels). The user can manually define where and which elements should
	be placed. This is given by the 'elem' list argument. This list has to follow
	a predefined structure: each element in a list is it self a list with 2 slots:
	1) a vector with X,Y position in map coordinates; and 2) the pattern to be
	used. The patterns can any that is available in the 'elements' object or
	manually defined as a vector of T and F indicating the state of the cell. The
	two requirements as that it follows a square with odd number of cells in the
	side, even if the pattern does not require it. This allows to easily place
	the pattern with the X,Y as the center cell. For instance the vector
	'c(T, F, T, F, T, F, T, F, T)' creates a cross on a 3x3 matrix.

}
\value{
	A raster representing the alive/dead cells.
}
\author{
	Pedro Tarroso
}
\examples{
	require(raster)
	map <- raster(extent(-50, 50, -50, 50), nrows=50, ncols=50, vals=0)
	p1 <- prepareGrid(map)
	cross <- c(T, F, T, F, T, F, T, F, T)
	elem <- list(list(c(-25, 25), c(T, F, T, F, T, F, T, F, T)))
	p2 <- prepareGrid(map, elem)
	plot(stack(p1, p2))
}
