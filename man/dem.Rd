\name{dem}
\alias{dem}
\title{
	Global Digital Elevation Model
}
\description{
    A global digital elevation model based on SRTM
}
\details{
	A raster with the global elevation at 1degree spatial resolution.

}
\source{
	Based on SRTM downloaded from WorldClim (www.worldclim.org)
}

\examples{
	dem <- raster(system.file("data/dem.tif", package="gameR"))
	plot(dem)

}
