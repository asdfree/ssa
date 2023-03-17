# annual earnings.
# for pensioner payouts, see
# the '04 extract
library(haven)
library(httr)

tf <- tempfile()

ssa_url <- "https://www.ssa.gov/policy/docs/microdata/epuf/epuf2006_sas_files.zip"

GET( ssa_url , write_disk( tf ) )

ssa_files <- unzip( tf , exdir = tempdir() )

ssa_fn <- grep( 'demographic' , ssa_files , value = TRUE )

annual_fn <- grep( 'annual' , ssa_files , value = TRUE )

ssa_tbl <- read_sas( ssa_fn )

annual_tbl <- read_sas( annual_fn )

ssa_df <- data.frame( ssa_tbl )

annual_df <- data.frame( annual_tbl )

names( ssa_df ) <- tolower( names( ssa_df ) )

names( annual_df ) <- tolower( names( annual_df ) )
summed_earnings_5152 <-
	with( 
		subset( annual_df , year_earn %in% 1951:1952 ) , 
		aggregate( annual_earnings , list( id ) , sum )
	)
	
names( summed_earnings_5152 ) <- c( 'id' , 'tot_cov_earn5152' )

summed_earnings_5306 <-
	with( 
		subset( annual_df , year_earn > 1952 ) , 
		aggregate( annual_earnings , list( id ) , sum )
	)
	
names( summed_earnings_5306 ) <- c( 'id' , 'tot_cov_earn5306' )

summed_quarters_5306 <-
	with( 
		subset( annual_df , year_earn > 1952 ) , 
		aggregate( annual_qtrs , list( id ) , sum )
	)

names( summed_quarters_5306 ) <- c( 'id' , 'qc5306' )
earnings_2006 <- annual_df[ annual_df[ , 'year_earn' ] == 2006 , c( 'id' , 'annual_earnings' ) ]

names( earnings_2006 ) <- c( 'id' , 'tot_cov_earn06' )
stopifnot( all( !is.na( ssa_df ) ) )

before_nrow <- nrow( ssa_df )

ssa_df <- merge( ssa_df , summed_earnings_5152 , all.x = TRUE )

ssa_df <- merge( ssa_df , summed_earnings_5306 , all.x = TRUE )

ssa_df <- merge( ssa_df , summed_quarters_5306 , all.x = TRUE )

ssa_df <- merge( ssa_df , earnings_2006 , all.x = TRUE )

ssa_df[ is.na( ssa_df ) ] <- 0

stopifnot( nrow( ssa_df ) == before_nrow )

# ssa_fn <- file.path( path.expand( "~" ) , "SSA" , "this_file.rds" )
# saveRDS( ssa_df , file = ssa_fn , compress = FALSE )
# ssa_df <- readRDS( ssa_fn )
ssa_df <- 
	transform( 
		ssa_df ,

		decade_of_birth = floor( yob / 10 ) * 10 ,
		
		sex = factor( sex , levels = 1:2 , labels = c( 'male' , 'female' ) ) ,
		
		tot_cov_earn3706 = ( tot_cov_earn3750 + tot_cov_earn5152 + tot_cov_earn5306 ) ,
		
		qc3706 = ( qc3750 + qc5152 + qc5306 ) ,
		
		any_earnings_2006 = ( tot_cov_earn06 > 0 ) ,
		
		earnings_periods =
			factor(
				ifelse( ( tot_cov_earn5152 + tot_cov_earn5306 > 0 ) & tot_cov_earn3750 > 0 , 1 ,
				ifelse( tot_cov_earn5152 > 0 | tot_cov_earn5306 > 0 , 2 ,
				ifelse( tot_cov_earn3750 > 0 , 3 , 4 ) ) ) ,
				levels = 1:4 ,
				labels =
					c( 'Earnings in both periods' , 'Earnings during 1951-2006 only' ,
						'Earnings during 1937-1950 only' , 'No earnings' ) )
	)
nrow( ssa_df )

table( ssa_df[ , "sex" ] , useNA = "always" )
mean( ssa_df[ , "tot_cov_earn3706" ] )

tapply(
	ssa_df[ , "tot_cov_earn3706" ] ,
	ssa_df[ , "sex" ] ,
	mean 
)
prop.table( table( ssa_df[ , "decade_of_birth" ] ) )

prop.table(
	table( ssa_df[ , c( "decade_of_birth" , "sex" ) ] ) ,
	margin = 2
)
sum( ssa_df[ , "tot_cov_earn3706" ] )

tapply(
	ssa_df[ , "tot_cov_earn3706" ] ,
	ssa_df[ , "sex" ] ,
	sum 
)
quantile( ssa_df[ , "tot_cov_earn3706" ] , 0.5 )

tapply(
	ssa_df[ , "tot_cov_earn3706" ] ,
	ssa_df[ , "sex" ] ,
	quantile ,
	0.5 
)
sub_ssa_df <- subset( ssa_df , qc3706 >= 40 )
mean( sub_ssa_df[ , "tot_cov_earn3706" ] )
var( ssa_df[ , "tot_cov_earn3706" ] )

tapply(
	ssa_df[ , "tot_cov_earn3706" ] ,
	ssa_df[ , "sex" ] ,
	var 
)
t.test( tot_cov_earn3706 ~ any_earnings_2006 , ssa_df )
this_table <- table( ssa_df[ , c( "any_earnings_2006" , "decade_of_birth" ) ] )

chisq.test( this_table )
glm_result <- 
	glm( 
		tot_cov_earn3706 ~ any_earnings_2006 + decade_of_birth , 
		data = ssa_df
	)

summary( glm_result )
library(dplyr)
ssa_tbl <- as_tibble( ssa_df )
ssa_tbl %>%
	summarize( mean = mean( tot_cov_earn3706 ) )

ssa_tbl %>%
	group_by( sex ) %>%
	summarize( mean = mean( tot_cov_earn3706 ) )
library(data.table)
ssa_dt <- data.table( ssa_df )
ssa_dt[ , mean( tot_cov_earn3706 ) ]

ssa_dt[ , mean( tot_cov_earn3706 ) , by = sex ]
chart_five_results <- prop.table( table( ssa_df[ , 'earnings_periods' ] ) )
chart_five_results <- round( 100 * chart_five_results )

stopifnot( chart_five_results[ 'Earnings in both periods' ] == 16 )
stopifnot( chart_five_results[ 'Earnings during 1951-2006 only' ] == 55 )
stopifnot( chart_five_results[ 'Earnings during 1937-1950 only' ] == 4 )
stopifnot( chart_five_results[ 'No earnings' ] == 25 )
nonzero_2006_earners <- ssa_df[ ssa_df[ , 'tot_cov_earn06' ] > 0 , 'tot_cov_earn06' ]
stopifnot( round( mean( nonzero_2006_earners ) , 0 ) == 30953 )
stopifnot( round( quantile( nonzero_2006_earners )[ 3 ] , 0 ) == 24000 )
nonzero_2006_earners <- ssa_df[ ssa_df[ , 'tot_cov_earn06' ] > 0 , ]
stopifnot( round( mean( nonzero_2006_earners[ , 'tot_cov_earn06' ] ) , 0 ) == 30953 )
stopifnot( round( quantile( nonzero_2006_earners[ , 'tot_cov_earn06' ] )[ 3 ] , 0 ) == 24000 )
stopifnot( round( nrow( nonzero_2006_earners ) * 100 , -3 ) == 156280000 )
earners_in_2006_by_sex <- table( nonzero_2006_earners[ , 'sex' ] ) * 100
stopifnot( round( earners_in_2006_by_sex[ 'male' ] , -3 ) == 81576000 )
stopifnot( round( earners_in_2006_by_sex[ 'female' ] , -3 ) == 74681000 )
