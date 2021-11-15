# gameR

The package *gameR* is meant to be a sink of useless code devoted to games in R.
There are two rules in this package:
 1. The games in the package must include, to any extent, data that we use in 
 everyday R analyses.
 2. No rule is meant to be forever with the exception of rule no. 2

## Games

1. Game of Life
2. Eat Your Data

## Installation

There are several option to install *gameR*:

- To install the current released version found [here](https://github.com/ptarroso/gameR/releases), you need to
  download the built *tar.gz* package file and install it.

- Clone the repository with *git* and build the package in your computer.

- Use the *devtools* package in R. Example code:

```
    library(devtools)
    install_github("ptarroso/gameR", subdir="source")
```

This package depends on other packages:
- [keypress](https://cran.r-project.org/web/packages/keypress/index.html)
- [raster](https://cran.r-project.org/web/packages/raster/index.html)

## Usage

The Game of Life depends on keypress so it will only work on a command line session.
On linux and Mac should be fairly simple: open up a terminal and start a session
by typing `R` at the prompt. In Windows it will probably work similarly, if you
open the command line (`cmd` in search bar) and start the `R.exe` in the 
installation directory. Typically `C:\Program Files\R\R-4.0.0\bin\R.exe`, depending
on the R version you have installed. You can check installation directory with 
the R command `.libPaths()`. (Note: I did not test yet the package in Windows, 
so I'm guessing it will work...)

The keypress is detecting key input in the console, so you have to move the focus 
to your console window to be able to play.

The "eat your data" game does not have the dependency on keypress, thus it must work
on most platforms. The focus must be on the graphical window, so, you probably
don't have to do anything else once the game starts.


### Game of Life:
```
library(gameR)
gol()
```

### Eat Your Data
```
library(gameR)
eatyourdata(iris[,1:4])
```


