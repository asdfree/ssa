# 
# 
# 
library(haven)

tf <- tempfile()

ssa_url <- "https://www.ssa.gov/policy/docs/microdata/epuf/epuf2006_sas_files.zip"

download.file( ssa_url , tf , mode = 'wb' )

ssa_files <- unzip( tf , exdir = tempdir() )

demographic_fn <- grep( 'demographic' , ssa_files , value = TRUE )

annual_fn <- grep( 'annual' , ssa_files , value = TRUE )

demographic_df <- read_sas( demographic_fn )

annual_df <- read_sas( annual_fn )
# ssa_fn <- file.path( path.expand( "~" ) , "SSA" , "this_file.rds" )
# saveRDS( ssa_df , file = ssa_fn , compress = FALSE )
# ssa_df <- readRDS( ssa_fn )
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
ssa_tbl <- as_tibble( ssa_df )
ssa_tbl %>%
	summarize( mean = mean( fpmt ) )

ssa_tbl %>%
	group_by( stat ) %>%
	summarize( mean = mean( fpmt ) )
