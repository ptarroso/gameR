\name{Game of Life}
\alias{gol}
\title{
	Start a Game of Life (with a few extra rules)
}
\description{
    The John Conway's Game of Life has only two rules... That was not enough for
    gameR...
}
\usage{
gol(map=NULL, grid=NULL, mean=NA, sd=NA)
}
\arguments{
  \item{map}{
    A raster map defining your world. This is a 'raster' object (see the raster
		package) that defines the area where the Game of Life will unfold. It can
		define a suitable area (land) with data and non-suitable (ocean) with NA. It
		can also have values that when combined with the mean and sd further details
		living areas based on a normal distribution. If NULL, a rectangular map is
		created, covering the extent (-180, 180, -90, 90) with 2 degrees of
		resolution.
	}
  \item{grid}{
    This is the grid with the living elements. It may be NULL and some elements
		are randomly placed in the map. It can be set to a raster object with the
		same proprieties as "map" but with only zeros and ones. Of course, the 1s
		define the living cells.
	}
	\item{mean}{
		If not NA, it is the mean value of a normal distribution that defines
		suitable values in relation to the 'map' data.
	}
	\item{sd}{
		If not NA, it is the standard deviation of a normal distribution that
		defines suitable values in relation to the 'map' data.
	}
}
\details{
	John Conways's idea was to simplify the game rules to the lowest number
	possible but still provide some complexity of a living automata. He
	ingeniously reduced the rules to only two:
		- If the cell is alive, then it stays alive if it has either 2 or 3 live
		  neighbors
		- If the cell is dead, then it springs to life only in the case that it has
		  3 live neighbors
	There is an implicit third rule that is all other cells remain dead or die in
	the following generation. But, of course, gameR needs more rules! So I made a
	few changes. First, the Game of Life happens in an infinite plane. Not
	anymore. It happens on a map! The two extra rules:
		- I have removed the infinite plane but gave them a world. So, at the edges
		  of the world, where land and water meet, the cells can now live forever
			where	they contemplate the infinity on an ocean.
		- Cells only live with certain environmental conditions, so they die if
		  those conditions are not met.

}
\value{
	Does not return anything... or maybe some joy and fun.
}
\references{
    https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
}
\author{
	Pedro Tarroso
}
\examples{
	# Simplest possible
	gol()

	# Use a world DEM
	dem <- raster(system.file("data/dem.tif", package="gameR"))
	gol(dem, mean=500, sd=250)

}
