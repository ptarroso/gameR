\name{ineigh}
\alias{ineigh}
\title{
	Get neighbor indices
}
\description{
    A fast function to get neighbor indices.
}
\usage{
ineigh(i, n, nrow, ncol)
}
\arguments{
  \item{i}{
    Source pixel ID number.
	}
  \item{n}{
    Number of neighbors to get. For a 3x3 neighborhood, this value is set to 1
		(a single neighbor to each of the eight sides of the pixel).
	}
	\item{nrow}{
		The number of rows in the raster.
	}
	\item{ncol}{
		The number of columns in the raster.
	}
}
\details{
	This is a fast function to get the indices of the neighbors to a pixel. It
	does not require the matrix/raster object, only the number of rows and
	columns, to find the neighbors. It returns the sorted indices for the pixel,
	including the	focal pixel. If at the edge, it returns NAs so the length is
	always the number of neighbors asked.

}
\value{
	A vector with integer indices.
}
\author{
	Pedro Tarroso
}
\examples{
	# the 3x3 neighbors of pixel 34, on a matrix with 200 rows and columns
	ineigh(34, 1, 200, 200)

}
