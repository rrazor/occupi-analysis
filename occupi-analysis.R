library( ggplot2 )
library( gridExtra )
library( scales )

load_data <- function ( ) {
	d <- read.csv( "data/occupi-readings.csv", header=T, sep="\t" )
	return( d )
}

