library( ggplot2 )
library( gridExtra )
library( scales )

#
# Load raw occupi data from data/ directory
#
load_data <- function ( ) {
	d <- read.csv( "data/occupi-readings.csv", header=T, sep="\t" )
	return( d )
}

#
# Process raw data into factors and types
#
process_data <- function ( d ) {
	d$room_id <- as.factor( d$room_id )
	d$room_id <- factor( d$room_id, levels=c( "1", "2", "3", "4" ), labels=c( "Quad 1", "Quad 2", "Quad 3", "Quad 4" ) )
	
	d$dow <- as.factor( d$dow )
	d$dow <- factor( d$dow, levels=c( "0", "1", "2", "3", "4", "5", "6" ), labels=c( "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" ) )
	
	d$timestamp <- as.POSIXct( d$timestamp, origin="1970-01-01" )
	return( d )
}

#
# Load & return processed data (proper factors, etc.)
#
ready_data <- function ( d ) {
	d <- load_data( )
	d <- process_data( d )
	return( d )
}


#
# Global theme used in plot_ functions
#
my_theme <- function ( ) {
	t <- theme_grey( ) + 
		theme(
		    text = element_text( family="Avenir Book", colour="#333333" ),
			panel.background = element_blank( ),
			panel.grid.major.y = element_blank( ),
			panel.grid.minor.y = element_blank( ),
			panel.grid.major.x = element_blank( ),
			panel.grid.minor.x = element_blank( ),
			plot.margin = unit( c(.2, .2, .2, .2 ), "inches" )
		)
	return( t )
}

#
# Add a "fake" timestamp on a single day, putting all days on a single
# day's timescale
#
build_one_day_ts <- function ( d ) {
	return( as.POSIXct( (d$hour * 60 + d$minute)*60, origin="2013-06-01" ) )
}

#
# Questions:

# 1. Which quad is the most popular?  (Quad 2!)
plot_quad_popularity <- function ( d ) {
	return(
		ggplot( d, aes( x=room_id ) ) +
		geom_bar( aes( fill=ifelse(room_id=="Quad 2", T, NA ) ) ) +
		scale_x_discrete( name="Room" ) +
		scale_y_continuous( labels=comma, name="Readings", limits=c(0,200000), breaks=c(0,100000,200000) ) +
		scale_fill_discrete( guide=F ) +
		my_theme( ) +
		ggtitle( "Positive Readings by Quad, All Time" )
	)
}

# 2. Which time of day is the most popular, across all quads?
plot_all_quad_time_hist <- function ( d ) {
	d$one_day_ts <- build_one_day_ts( d )
	
	return(
		ggplot( d, aes( x=one_day_ts ) ) +
		geom_histogram( binwidth=15*60 ) +
		scale_x_datetime( name="Time of Day" ) +
		scale_y_continuous( labels=comma, name="Readings" ) +
		my_theme( ) +
		ggtitle( "Positive Readings by Time of Day" )
	)	
}

# 2a. Is any one quad popular at a particular time?  Unpopular?
plot_all_quad_time_density <- function ( d ) {
	d$one_day_ts <- build_one_day_ts( d )
	
	return(
		ggplot( d, aes( x=one_day_ts, colour=room_id ) ) +
		geom_density( binwidth=15*60 ) +
		scale_x_datetime( name="Time of Day" ) +
		scale_y_continuous( labels=comma, name="Reading Density" ) +
		scale_colour_discrete( name="Room" ) +
		my_theme( ) +
		ggtitle( "Positive Readings by Time of Day" )
	)
}
plot_all_quad_time_freqpoly <- function ( d ) {
	d$one_day_ts <- build_one_day_ts( d )
	
	return(
		ggplot( d, aes( x=one_day_ts, colour=room_id ) ) +
		geom_freqpoly( binwidth=15*60 ) +
		scale_x_datetime( name="Time of Day" ) +
		scale_y_continuous( labels=comma, name="Reading Density" ) +
		scale_colour_discrete( name="Room" ) +
		my_theme( ) +
		ggtitle( "Occupancy Readings by Time of Day" )
	)
}

# 3. What are the most active days in the quads?
plot_all_quad_day_hist <- function ( d ) {
	return(
		ggplot( d, aes( x=timestamp ) ) +
		geom_histogram( binwidth=86400 ) +
		scale_x_datetime( name="Day" ) +
		scale_y_continuous( labels=comma, name="Readings" ) +
		my_theme( ) +
		ggtitle( "Positive Readings by Date" )
	)	
}

# 4. Is the positive reading count a correct proxy for occupancy?
# 4a. How are the numbers of positive readings distirbuted for a 15-minute interval?
plot_reading_counts_by_quarter_hour <- function ( d ) {
	d.counts <- aggregate( x=d$room_id, by=list( date=d$date, room_id=d$room_id, hour=d$hour, minute=as.integer(d$minute/15)*15 ), FUN=length )
	d.counts$one_day_ts <- build_one_day_ts( d.counts )
	return(
		ggplot( d.counts, aes( x=x ) ) +
		geom_histogram( binwidth=10 ) +
		scale_x_continuous( name="Positive Readings", limits=c( 0, 1000 ) ) +
		scale_y_continuous( labels=comma, name="Frequency" ) +
		my_theme( ) +
		ggtitle( "Number of Positive Readings in 15-minute Intervals" )
	)
}

# 4b. What does the active days graph look like excluding intervals with < 50 positive readings?
plot_15_min_positive_count_time_hist <- function ( d ) {
	d.counts <- aggregate( x=d$room_id, by=list( date=d$date, room_id=d$room_id, hour=d$hour, minute=as.integer(d$minute/15)*15 ), FUN=length )
	d.counts$date <- as.POSIXct( d.counts$date, format="%Y-%m-%d" )
	d.counts$one_day_ts <- build_one_day_ts( d.counts )
	d.counts.over.50 <- subset( d.counts, x > 50 )
	return(
		ggplot( d.counts.over.50, aes( x=one_day_ts ) ) +
		geom_histogram( binwidth=15*60 ) +
		scale_x_datetime( name="Time of Day" ) +
		scale_y_continuous( labels=comma, name="Periods" ) +
		my_theme( ) +
		ggtitle( "Occupied Periods by Time of Day" )
	)
}

# 4c. Does the "occupied days" count compare to the "positive readings" histogram?
plot_compare_reading_counts_to_occupied_periods <- function ( d ) {
	return( grid.arrange( plot_all_quad_time_hist( d ), plot_15_min_positive_count_time_hist( d ), nrow=2, ncol=1 ) )
}

# 5. Revisit 2a.: Is any one quad popular at a particular time?
plot_grid_hour_by_quad <- function ( d ) {
	d$one_day_ts <- build_one_day_ts( d )
	
	return(
		ggplot( d, aes( x=one_day_ts, y=room_id ) ) +
		scale_x_datetime( name="Time of Day", limits=c( as.POSIXct( "2013-06-01 09:00" ), as.POSIXct( "2013-06-01 18:00" ) ) ) +
		stat_bin( aes( fill=..count.. ), binwidth=15*60, geom="tile", position="identity" ) +
		scale_y_discrete( name="Room" ) +
		scale_fill_continuous( high="red", low="white", name="Positive Readings", limits=c( 0, 10000 ), labels=comma ) +
		my_theme( ) +
		ggtitle( "Positive Readings by Room and Time of Day" )
	)
}

# 6. Are certain combinations of weekday and time popular?
plot_grid_hour_by_dow <- function ( d ) {
	d$one_day_ts <- build_one_day_ts( d )
	
	return(
		ggplot( d, aes( x=one_day_ts, y=dow ) ) +
		scale_x_datetime( name="Time of Day", limits=c( as.POSIXct( "2013-06-01 09:00" ), as.POSIXct( "2013-06-01 18:00" ) ) ) +
		stat_bin( aes( fill=..count.. ), binwidth=15*60, geom="tile", position="identity" ) +
		scale_y_discrete( name="Day of Week", labels=c("", "Mon", "", "Wed", "", "Fri", "" ) ) +
		scale_fill_continuous( high="red", low="white", name="Positive Readings", limits=c( 0, 10000 ), labels=comma ) +
		my_theme( ) +
		ggtitle( "Positive Readings by Day of Week and Time of Day" )
	)
}

# 7. Is there a way to see quad/day of week/time combinations?
plot_grid_hour_by_dow_facet <- function ( d ) {
	return(
		plot_grid_hour_by_dow( d ) +
		facet_grid( room_id ~ . ) +
		scale_fill_continuous( high="red", low="white", name="Positive Readings", labels=comma )
	)
}

# 8. What about a calendar-like view?
# This does not work yet
# plot_grid_calendar <- function ( d ) {
	d$one_day_ts <- build_one_day_ts( d )
	
	x_breaks <- seq( min( as.POSIXct( d$date ) ), max( as.POSIXct( d$date ) ), by=86400 )
	y_breaks <- seq( as.POSIXct( "2013-06-01 09:00" ), as.POSIXct( "2013-06-01 18:00" ), by=15*60 )
	
	return(
		ggplot( d, aes( x=timestamp, y=one_day_ts ) ) +
		scale_x_datetime( name="Date" ) +
		scale_y_datetime( name="Time of Day", limits=c( as.POSIXct( "2013-06-01 09:00" ), as.POSIXct( "2013-06-01 18:00" ) ) ) +
		stat_bin2d( breaks=list( x=x_breaks, y=y_breaks ) ) +
		scale_fill_continuous( high="red", low="white", name="Positive Readings", labels=comma ) +
		my_theme( ) +
		ggtitle( "Positive Readings Calendar" )
	)
}