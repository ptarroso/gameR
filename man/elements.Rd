\name{elements}
\alias{elemtns}
\title{
	Patterns for Game of Life
}
\description{
    A list of pre-built elements patterns to be used in Game of Life.
}
\details{
	A list with 7 element patterns to be used in the Game of Life. Random
	generation of patterns with function 'prepareGrid' uses this list to fill a
	grid with patterns. Can be used to manmually create an 'elem' list for then
	'prepareGrid' function (see 'prepareGrid' help).

	In this function there are:
	    - glider with  3x3 pattern
			- lwspaceship with  5x5 pattern
			- fpentomino with  3x3 pattern
			- bheptomino with  5x5 pattern
			- piheptomino with  3x3 pattern
			- acorn with  7x7 pattern
			- gosper.glidergun with  37x37 pattern

	Although real pattern might be smaller, all patterns are squared with an odd
	number of cells in the side.

}
\source{
	Based on SRTM downloaded from WorldClim (www.worldclim.org)
}
