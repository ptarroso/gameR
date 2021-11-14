black2red <- function(i, n) {
		# Simple wrapping function to return a gradient color from black to red
    col <- colorRampPalette(c("#212224", "#FF1A1A"))
    return(col(n)[i])
}

eatyourdata <-
function(data, ndim=4) {

	if (ncol(data) < ndim) {
		stop(paste("data must have at least", ndim, "dimensions."))
	}

	resetgame <- function(g) {
	    g$state <- 0 # STate of the game (start, game, over)
	    g$u <- 0.025 # Unit of snake size
	    g$snake <- matrix(rep(0, 3*ndim), 3, ndim) # snake
	    g$score <- NULL  # Tracking score
			g$growth <- 1 # Controls snake growth (growth proportional to score)
	    g$move <- NULL # Move direction
	    g$dims <- NULL # Components (dimensions) currently active
			g$comp <- 0 # Tracks component label animation
	    g$food <- NULL # Food (data points) available
			g$item <- NULL # Current food item to eat
	    g$eat <- TRUE # Tracks if snake ate on current frame
	    g$time <- Sys.time() # Tacks time for animation purposes
	}

	startscreen <- function(g) {
        dev.hold()
        plot.new()
        plot.window(c(-1,1), c(-1,1), asp=1)
        text(0, 0.5, "EAT YOUR DATA", cex=4, font=2, pos=3)
        text(0, 0.3, "A snake game with your data", cex=2, font=3, pos=3)
        text(0, -0.2, "Press S to start", col='red', pos=3, cex=1.5)
        dev.flush()
	}

    startgame <- function(g) {
	    g$snake[2,1] <- g$snake[2,1] - g$u
	    g$snake[3,1] <- g$snake[3,1] - 2*g$u

	    g$score <- 0
	    g$move <- sample(c("Right", "Up", "Down"), 1)
	    g$dims <- c(1,2)

	    g$food <- predict(g$pca)
	    g$food <- (g$food - min(g$food))/diff(range(g$food))*2-1
	    g$food <- g$food - g$food%%g$u
    }

    gameover <- function(g) {
        dev.hold()
        rect(g$l[1], g$l[3], g$l[2], g$l[4], col="white")
        t <- as.numeric( Sys.time() - g$time, units="secs")
        if (t < 2) {
            text(0, 0.3, "GAME OVER", cex=t*2, col='red')
        } else if (t < 10) {
            text(0, 0.3, "GAME OVER", cex=4, col='red')
            text(0, -0.2, "You lost but it was impossible to win with such good data!", cex=1.5)
            text(0, -0.7, paste("You ate", g$score, "data points!"))
        } else {
            g$state = 0
        }
        dev.flush()
    }

    movesnake <- function(g) {
	    head <- g$snake[1,]

	    # Move snake head
	    if (g$move == "Right") {
		    head[g$dims[1]] <- head[g$dims[1]] + g$u
	    } else if (g$move == "Left") {
		    head[g$dims[1]] <- head[g$dims[1]] - g$u
	    } else if (g$move == "Up") {
		    head[g$dims[2]] <- head[g$dims[2]] + g$u
	    } else if (g$move == "Down") {
		    head[g$dims[2]] <- head[g$dims[2]] - g$u
	    }

	    # Update body position
	    g$snake <- rbind(head, g$snake)

      # Eat if triggered in previous move
	    if (g$eat) {
		    i <- sample(1:nrow(g$food), 1)
		    g$item <- g$food[i,]
		    g$food <- g$food[-i,]
		    g$eat <- FALSE
	    }

		# Check if snake ate
		if (all(round(head, 4) == round(g$item, 4))) {
			g$score <- g$score + 1
			g$eat <- TRUE
			g$growth <- g$score
		} else {
			if (g$growth == 1) {
				# Remove tail end (does not grow)
				g$snake <- g$snake[-nrow(g$snake),]
			} else {
				# Grow based on growth factor
				g$growth <- g$growth - 1
			}
		}

	}

    checkcollision <- function(g) {
		# Collision detection
		head <- g$snake[1,]
		if (any(head[g$dims]-g$u/2 <= g$l[c(1,3)]) | any(head[g$dims]+g$u/2 >= g$l[c(2,4)])) {
			g$state <- 3
		}
		tail <- g$snake[-1,]
		if (any(tail[,g$dims[1]] == head[g$dims[1]] & tail[,g$dims[2]] == head[g$dims[2]])) {
			g$state <- 3
		}
    }

    drawsnake <- function(g) {
        dev.hold()
        # Plot game area
        g$l <- par("usr")

        rect(g$l[1], g$l[3], g$l[2], g$l[4], col="white")
        abline(v=0)
        abline(h=0)

				# Component label display and animation
				ccex <- 2+g$comp**2/25
				ccol <- black2red(g$comp+1, 6)
				if (g$comp > 0) g$comp <- g$comp - 1
				text(-0.99, 0.05, paste0("C", g$dims[1]), cex=ccex, col=ccol)
				text(0.07, 1.01, paste0("C", g$dims[2]), cex=ccex, col=ccol)

        rect(g$l[1], g$l[3], g$l[2], g$l[4], lwd=2)

        # Plot food
        points(g$food[,g$dims[1]], g$food[,g$dims[2]],
               cex=0.5, col="lightblue", pch=16)
        points(g$item[g$dims[1]], g$item[g$dims[2]],
               cex=1, col="red", pch=16)
        legend("topleft", legend=paste("Score:", g$score))

        # Plot snake
        rect(g$snake[,g$dims[1]]-g$u/2, g$snake[,g$dims[2]]-g$u/2,
             g$snake[,g$dims[1]]+g$u/2, g$snake[,g$dims[2]]+g$u/2,
             col=c("orange", "orange2"), border=NA)
        dev.flush()
    }


    keyPress <- function(key, g) {
        if (key == "q") {
            if (g$state == 0) {
                dev.off()
                return(invisible(1))
            } else {
                g$state <- 0
            }
        } else if ( key == "s") {
            if (g$state == 0) {
                g$state <- 1
                startgame(g)
            }
        } else if (key %in% c("Left", "Right", "Up", "Down")) {
            g$move <- key
        } else if (key %in% as.character(1:(ndim-1))) {
	        g$dims <- c(as.integer(key), as.integer(key)+1)
	        g$comp <- 5 # Sets duration for component label animation
        }
        NULL
    }


    idle <- function(x, g) {
        if (g$state == 0) {
            resetgame(g)
            startscreen(g)
        } else if (g$state == 1) {
            if (as.numeric( Sys.time() - g$time, units="secs") > 0.15) {
                movesnake(g)
                checkcollision(g)
                drawsnake(g)
              	g$time <- Sys.time()
            }
        } else if (g$state == 3) {
            gameover(g)
        }
        NULL
    }

	genv <- new.env(parent = emptyenv())
	genv$pca <- princomp(data)
	genv$highscore <- 0
    resetgame(genv)

    dev.new()
    par(mar=rep(0,4))
    getGraphicsEvent("Eat Your Data",
                     onKeybd = function(x) keyPress(x, genv),
                     onIdle = function(x) idle(x, genv))
}
