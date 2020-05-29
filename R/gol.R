ineigh <- function(i, n, nrow, ncol) {
	# Calculates the index of n neighbors to cell (includes the cell in result)
	side <- n*2+1
	a <- matrix((i-n):(i+n), side, side, byrow=T)
	b <- matrix(n:-n, side, side)*ncol
	ineigh <- a-b
	ineigh[ineigh<=0 | ineigh > ncol*nrow] <- NA
	if ((i-1)%%ncol < n) {
		ineigh[,1:abs((i%%ncol)-n-1)] <- NA
	}
	if ((i+n)%%ncol %in% 1:n) {

		ineigh[,(side+1-(i+n)%%ncol):side] <- NA
	}
	return(ineigh[order(ineigh)])
}

rneigh <- function(i, raster, neigh=1, rm.focal=T) {
	# A simple wraper function for ineigh with a raster
	ng <- ineigh(i, neigh, nrow(raster), ncol(raster))
	if (rm.focal) return(ng[ng!=i])
	return(ng)
}

.glider <- rep(F, 3**2)
.glider[c(1:4,8)] <- T

.lwspaceship <- rep(F, 5**2)
.lwspaceship[c(2,5,6,11,15:18)] <- T

.fpentomino <- rep(F, 3**2)
.fpentomino[c(2:5,8)] <- T

.bheptomino <- rep(F, 5**2)
.bheptomino[c(6,8,9,11:13,17)] <- T

.piheptomino <- rep(F, 3**2)
.piheptomino[c(1:4,6,7,9)] <- T

.acorn <- rep(F, 7**2)
.acorn[c(16,25,29,30,33:35)] <- T

.gosper.glidergun <- rep(F, 37**2)
.gosper.glidergun[c(543, 578, 580, 605, 606, 613, 614, 627, 628, 641,
                    645, 650, 651, 664, 665, 667, 668, 677, 683, 687,
                    688, 704, 705, 714, 718, 720, 721, 726, 728, 751,
                    757, 765, 789, 793, 827, 828)] <- T

elements <- list(glider=.glider, lwspaceship=.lwspaceship, fpentomino=.fpentomino,
                 bheptomino=.bheptomino, piheptomino=.piheptomino, acorn=.acorn,
                 gosper.glidergun=.gosper.glidergun)


liferules <- function(lraster, nearNA=F) {
	# Famous Rules
	#  - If the cell is alive, then it stays alive if it has either 2 or 3
	#    live neighbors
	#  - If the cell is dead, then it springs to life only in the case that it
	#    has 3 live neighbors
	# Less Famous Rules
	#  - If cell is near an NA (assuming it is water/ocean), then it lives
	#    forever to contemplate the infinity (raster edges at bbox are not
	#    considered for this)
	nextgen <- lraster * 0
	alive <- which(lraster[] == 1)
	for (i in alive) {
		neigh <- rneigh(i, lraster)
		nalive <- lraster[neigh] == 1
		# Check first rule
		if (sum(nalive) %in% 2:3) {
			nextgen[i] <- 1
		}

  	if (nearNA) {
			edge <- sum(is.na(neigh)) == 0
		}

		# Now check second rule
		# (only worth checking on dead around live ones)
		for (j in neigh[!nalive]) {
			if (!is.na(j)) {
				jneigh <- rneigh(j, lraster)
				if (sum(jneigh %in% alive) == 3) {
					nextgen[j] <- 1
				}
			} else if (is.na(j) & edge & nearNA) {
				nextgen[i] <- 1
			}
		}
	}
	return(nextgen)
}


envrules <- function(lraster, env, mean=0, sd=1, alpha=0.05) {
	# Environmental rules based on a normal distribution. Cell dies if outside the
	# the interval (alpha, 1-alpha)
	alive <- which(lraster[]==1)
	enva <- env[alive]
	p <- pnorm(enva, mean, sd)
	dead <- ifelse(p< alpha | p > (1-alpha), T, F)
	dead[is.na(dead)] <- T
	lraster[alive[dead]] <- 0
	return(lraster)
}

.start.menu.gol <- function() {
	plot.new()
	text(0.5, 0.8, "Game of Life", cex=4)
	text(0.5, 0.65, "in a raster map", cex=2, col='red')
	text(0.5, 0.3, paste("Instructions:\n",
	                     "This is a no-player game, so you just sit and watch!\n",
	                     "If things get a bit monotonous, press 'r' to add some",
	                     " randomness.\n",
	                     " Press 'q' or 'Ctrl+c' to quit"))
	text(0.5, 0.1, "Click on the console and press 's' to start")
}

prepareGrid <- function(map, elem=list(), maxiter=1000, n=20) {
	grid <- map * 0
	if ( length(elem) == 0 ) {
		## place some items randomly in map
		pixels <- which(!is.na(map[]))
		for (i in sample(length(elements), n, replace=T)) {
			e <- elements[[i]]
			# Loop to guarantee that element fits in the place
			# Does not gurantee that elements do not overlap, thow
			# If map is very small maxiter is certainly reached...
			iter <- 0 # Iteration per element
			flag <- TRUE
			while(flag) {
				iter <- iter + 1
				px <- sample(pixels, 1)
				ng <- rneigh(px, grid, (sqrt(length(e))-1)/2, F)
				if (sum(is.na(ng)) == 0) { # Is the px at raster limit?
					if (sum(is.na(grid[ng])) == 0 ) { # Does it have NAs?
						flag <- FALSE
					}
				}
				flag <- flag & iter < maxiter
			}
			if (iter < maxiter) {
				# Only place if maxiter is not reached
				grid[ng][e] <- 1
			} 
		}
	} else {
		# elem are given by user at specified locations in map coordinates
		# each item in list is a list with [[1]] c(X, Y); [[2]] element
		for (i in length(elem)) {
			if (length(elem[[i]][[1]]) == 2) {
				px <- cellFromXY(map, elem[[i]][[1]])
			} else {
				stop("Location of element ", i, " is not a pair of coordinates")
			}
			if (length(elem[[i]][[2]]) %% 2 == 1) {
				e <- elem[[i]][[2]]
			} else {
				stop("Element", i, "has a even number of rows/columns.")
			}
			ng <- rneigh(px, grid, (sqrt(length(e))-1)/2, F)
			if (length(e) != length(ng)) {
				stop("Placing element", i, "failed. Near raster limit?")
			}
			grid[ng][e] <- 1
		}
	}
	return(grid+map*0)
}


gol <- function(map=NULL, grid=NULL, mean=NA, sd=NA) {

	# Prepare data in respect to the arguments provided.
	if (is.null(grid) & is.null(map)) {
		# Function called without arguments. Create a simple map and populate
		map <- raster(resolution= c(2,2), vals=1)
		curgen <- prepareGrid(map)
	} else if (is.null(grid) & !is.null(map)) {
		# A map is provided. Populate the map randomly with elements
		curgen <- prepareGrid(map)
	} else if (!is.null(grid) & is.null(map)) {
		# A grid is provided. Just make sure it is zeros and ones.
		curgen <- (grid > 0)*1
		map <- curgen*0
	} else {
		curgen <- grid
	}

	# If mean and sd are provided, then add the extra "climate" rule
	extrarule <- FALSE
	if (!is.na(mean) & !is.na(sd)) {
		extrarule <- TRUE
	}

	# Some store data to avoid calculating each generation or key press
	ext <- as.vector(extent(map))
	pxdata <- which(!is.na(map[]))

	start <- TRUE

	while (TRUE) {
		if (start) {
			.start.menu.gol()
			gen <- 0
			k <- tolower(keypress(TRUE))
			if (k == "s") {
				start <- "FALSE"
			} else if (k == "q") {
				break
			}

		} else {
			k <- tolower(keypress(FALSE))
			if (k == "r") {
				curgen[sample(pxdata, length(pxdata)*0.01)] <- 1
			} else if (k == "q") {
				start <- TRUE
			}

			tmp <- curgen
			tmp[tmp[]==0] <- NA

			plot.new()
			plot.window(ext[1:2], ext[3:4], asp=1)
			image(map, col=hcl.colors(50, "Temps"), add=T)
			image(tmp, col='black', add=T)
			title(main=paste("Generation", gen))
			box()

			# Update life map
			curgen <- liferules(curgen, nearNA=T)
			if (extrarule) {
				curgen <- envrules(curgen, map, mean, sd)
			}
			gen <- gen + 1
		}
	}
}
