\name{Eat Your Data}
\alias{eyd}
\title{
	A snake game where you eat your own data.
}
\description{
    Have you ever look to you data in a table and thought "What the hell am I
		going to do with this"? Well, now you can play with it. Feed the snake in
		all dimensions of your data!
}
\usage{
eatyourdata(data, ndim=4)
}
\arguments{
  \item{data}{
    A table or matrix with your data. Must be numeric and without missing data.
		How could you play if the data were missing? Let's be serious.
	}
  \item{ndim}{
		You can set the maximum of dimensions (aka components) to play.
	}
}
\details{
	This implements a snake game where the objective is to eat the data points in
	a plot. However, if you data has more than 2 dimensions, well, you will have
	to 	navigate the snake in all the dimensions... this is not an easy task!
	But, no one knows your data better than you!


	The first digestion of your data is through a PCA. Then the game procees and
	is quite simple to play. Start the game pressing "S" key and then use	the
	arrows to navigate the snake. Numbers 1 to ndim-1 will allow to move to
	other dimensions: '1' plots first and second component, '2' plots second and
	third	and so on. You have to move the snake to the red data point in all
	dimensions so it can eat.
}
\value{
	Returns the satisfaction of seeing your data being used correctly!
}

\author{
	Pedro Tarroso
}
\examples{
	# Wit Iris dataset
	eatyourdata(iris[1:4])

}
