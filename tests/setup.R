if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

library(lodown)
lodown( "ssa" , output_dir = file.path( getwd() ) )
ssa_df <- readRDS( file.path( getwd() , "ssr_data/SSIPUF.rds" ) )

ssa_df <- 
	transform( 
		ssa_df , 
		
		mental_disorder = as.numeric( diag %in% 1:2 ) ,
		
		program_eligibility =
			factor( 
				prel , 
				
				levels = 0:5 , 
				
				labels =
					c( "Unspecified" ,
					"Aged individual" ,
					"Aged spouse" ,
					"Disabled or blind individual" ,
					"Disabled or blind spouse" ,
					"Disabled or blind child" )
			)
			
	)
	
nrow( ssa_df )

table( ssa_df[ , "stat" ] , useNA = "always" )
mean( ssa_df[ , "fpmt" ] )

tapply(
	ssa_df[ , "fpmt" ] ,
	ssa_df[ , "stat" ] ,
	mean 
)
prop.table( table( ssa_df[ , "program_eligibility" ] ) )

prop.table(
	table( ssa_df[ , c( "program_eligibility" , "stat" ) ] ) ,
	margin = 2
)
sum( ssa_df[ , "fpmt" ] )

tapply(
	ssa_df[ , "fpmt" ] ,
	ssa_df[ , "stat" ] ,
	sum 
)
quantile( ssa_df[ , "fpmt" ] , 0.5 )

tapply(
	ssa_df[ , "fpmt" ] ,
	ssa_df[ , "stat" ] ,
	quantile ,
	0.5 
)
sub_ssa_df <- subset( ssa_df , sex == "F" )
mean( sub_ssa_df[ , "fpmt" ] )
var( ssa_df[ , "fpmt" ] )

tapply(
	ssa_df[ , "fpmt" ] ,
	ssa_df[ , "stat" ] ,
	var 
)
t.test( fpmt ~ mental_disorder , ssa_df )
this_table <- table( ssa_df[ , c( "mental_disorder" , "program_eligibility" ) ] )

chisq.test( this_table )
glm_result <- 
	glm( 
		fpmt ~ mental_disorder + program_eligibility , 
		data = ssa_df
	)

summary( glm_result )
library(dplyr)
ssa_tbl <- tbl_df( ssa_df )
ssa_tbl %>%
	summarize( mean = mean( fpmt ) )

ssa_tbl %>%
	group_by( stat ) %>%
	summarize( mean = mean( fpmt ) )
