\name{rneigh}
\alias{rneigh}
\title{
	Get neighbor indices of a raster
}
\description{
    A wrapper for ineigh to get raster neighbor indices.
}
\usage{
rneigh(i, raster, neigh=1, rm.focal=T)
}
\arguments{
  \item{i}{
    Source pixel ID number.
	}
  \item{raster}{
    The raster object to extract row and column numbers.
	}
	\item{neigh}{
		The number of neighbors to get.
	}
	\item{rm.focal}{
		Remove the focal pixel from the results?
	}
}
\details{
	This is a wrapper for ineigh that simplifies getting the neighbors of a pixel
	in a raster object.

}
\value{
	A vector with integer indices.
}
\author{
	Pedro Tarroso
}
\examples{
	# the 3x3 neighbors of pixel 34, on a raster with 360 columns and 180 rows.
	require(raster)
	rst <- raster(nrow=180, ncol=360)
	rneigh(34, rst)

}
