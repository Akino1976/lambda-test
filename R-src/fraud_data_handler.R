#!/usr/bin/env Rscript
## load tables
options(scipen=999)
if( regexpr("windows", .Platform$OS.type, ignore.case = TRUE) > 0 )
{
	path 	<-  "C:/Users/se0725119/codes/bi-utils/src"
	HOME	<-  "C:/Users/se0725119"
	Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
	FULLPATH	<- path
} else if(regexpr("unix", .Platform$OS.type, ignore.case = TRUE) > 0) {
	FULLPATH		<- "/home/akino/code/bi-utils/src"
	FULLPATH		<- "/Users/akinoosx/ENV/bi-utils/src"
	path			<- FULLPATH
	HOME			<- "/Users/akinoosx"
	'.aws/credentials'
	## mac
	HOME 	<- pathSet( x = "$HOME" )
}
##########################################################################################
setwd(path)
source(
	list.files(
		pattern = "commonFunction.R",
		recursive = TRUE,
		full.names = TRUE
	)
)
if( file.exists(".Rprofile") ) { source(".Rprofile") }

R_files 	<- list.files(
	path = dirname(path),
	pattern = 'commonFunction.R',
	recursive = TRUE,
	full.names = TRUE,
	ignore.case = TRUE
)
source(R_files)
.pkg 		<- c(
	'data.table','aws.s3', 'ndjson', 'optparse', 'RODBC', 'readxl', 'stringi',
	'parallel', 'aws.signature','grid', 'Hmisc', 'uuid', 'ggpmisc', 'caret',
	 'bit64', 'scales', 'gridExtra', 'lubridate', 'ggplot2', 'fifer'
)
Object 	<- new("startUps", pkgs = .pkg , Input = c("data", "graf") )

Object$instant_pkgs( )

#######################################################
# PATHS
.HOME			<- dirname(FULLPATH)
.DATA			<- file.path(.HOME, 'DATA')
.GRAF			<- file.path(.HOME, 'GRAF')
.FRAUD		<- file.path('F:\\FRAUD')
#######################################################
get_data 	<- function(table_name, data_base = 'BamboraDW_Testing', query = NULL)
{
	con		<- odbcConnect(data_base)
	if( !is.null(query))
	{
		query <- query
	} else {
		query		<- sprintf("select * from [%s].[dbo].[%s]", data_base, table_name)
	}
	dataSet	<- sqlQuery(con, query)
	setDT(dataSet)
	close(con)
	return(dataSet)
}
search_and_replace <- function(pattern, keys)
{
	intital_position <- regexpr(pattern, keys)
	if( intital_position[1] > 0 )
	{
		query <- query
	} else {
		message('No match')
	}
}
fraud_cols	<- c(
	'currency',
	'additional_ref_no',
	'additional_ref_no2',
	'amount',
	'arn',
	'card_number',
	'card_scheme',
	'dba_name',
	'eci',
 	'issuer_country',
	'issuer_reason',
	'mcc',
 	'mcc_description',
 	'merchant_id',
	'org_no',
 	'reference_no',
 	'region',
 	'reported_date',
 	'sales_description',
 	'sales_location',
 	'sek_amount',
 	'ticket_no',
	'transaction_date',
	'trx_type',
	'dim_date',
	'nr_trx'
)


con		<- odbcConnect('RDS')
seq_dates	<- substring(
	seq(from = as.Date('2019-08-01'), to = as.Date('2019-10-01'), by = 'month') , 1,7)

for( .date in seq_dates)
{
	cat(strrep('#',50), '\n')
	cat('Running date ', .date, '\n')
	time <- system.time({
		file_name	<- sprintf('rds_clearing_fraud_%s', .date)
		query 	<- sprintf("SELECT * FROM [DW-Elixir].[dbo].[fn_return_fraud_clearing_view]('%s')", .date)
		dataSet	<- sqlQuery(con, query, as.is = c(rep(TRUE, 40) ))
		setDT(dataSet)
		dataSet[!is.na(fraud_created_at), is_fraud := 1L]
		save(dataSet, file = file.path(.FRAUD, paste0(file_name, '.RData')))
		#fwrite(dataSet, file = file.path(.FRAUD, paste0(file_name, '.csv')), sep = '\t')
	})[1:3]
	cat('Took ', time , '\n')
	rm(dataSet); gc(reset = TRUE)
}
close(con)

dataDT	<- fread("F:\\FRAUD/rds_clearing_fraud_2019-03.csv" , sep = '\t')
####################
# Common tables
currencyDT	<- get_data(table_name='curr_code', data_base = 'Datastore')
countryDT	<- fread(
	list.files(.DATA, pattern = 'country_codes.csv', full.names = TRUE)
)
setnames(countryDT, c('currency_code', 'currency_int_code'),
			  c('country_code', 'country_int_code'))
mcc_codes	<- read_excel(
	list.files(.DATA, pattern = 'mcc.*xls$', full.names = TRUE),
	skip = 1
)
setDT(mcc_codes)
setnames(mcc_codes, names(mcc_codes)[1:2], c('mcc', 'mcc_name'))
mcc_codes	<- mcc_codes[, .(mcc, mcc_name)]
entrymodeDT	<- get_data(table_name='entry_mode_desc', data_base = 'DW-Elixir')
idmethodDT  <- get_data(table_name='id_method_desc', data_base = 'DW-Elixir')
termcapDT	<- get_data(table_name='terminal_capability_desc', data_base = 'DW-Elixir')
.file_sales		<-list.files(path = .FRAUD, pattern = 'sales.RData', full.names = TRUE)

if(file.exists(.file_sales))
{
	load(.file_sales)
} else {
	salesDT	<- get_crm_data()
	.fil		<- list.files(path = .FRAUD, pattern = 'xlsx', full.names = TRUE)
	creditDT	<- read_excel(.fil)
	setDT(creditDT)

	#write.csv( salesDT[!is.na(Rating)], file = file.path(.FRAUD, 'credit.csv')
	salesDT	<- merge(salesDT, creditDT, by = 'OrgNr', all.x = TRUE)
	save(salesDT, file = file.path(.FRAUD, 'sales.RData'))
}

#############################################################
# Fraud analysis
#############################################################
exists('fraudDT') || load(file = file.path(.FRAUD, 'FraudDT.RData'))
exists('timeFraudDT') || load(file.path(.FRAUD, 'timeFraudDT.RData'))

fraudDT[,  ':=' (
	card_country = as.integer(card_country),
	is_econ = ifelse(!is.na(eci), 1, 0)
)]
setkey(countryDT, country_int_code)
setkey(fraudDT, transaction_country)
fraudDT[countryDT, trans_country := i.country_code]

setkey(countryDT, country_int_code)
setkey(fraudDT,  card_country)
fraudDT[countryDT, card_country_1 := i.country_code]
fraudDT[card_country != transaction_country , card_from_diff_country := 1L ]
fraudDT[is.na(card_from_diff_country), card_from_diff_country := 0L ]
AntalDT	<- fraudDT[, .(antal =.N), by = (card_from_diff_country)]
AntalDT[,  card_from_diff_country := factor(card_from_diff_country, levels = c(0,1), labels = c('Same','Other'))]
AntalDT[, Percentage := paste0(as.integer(antal/SUM(antal)*100), '%')]
PIE	<- ggplot(AntalDT, aes(x = '', y = antal, fill = card_from_diff_country )) +
			geom_bar(width = 1, stat = "identity")+
			coord_polar("y") +
			scale_fill_brewer(palette="Dark2") +
			scale_y_continuous( labels = Format) +
  			theme(axis.text.x=element_blank()) +
 			geom_text(aes(y = antal/2 + c(0, cumsum(antal)[-length(antal)]),
            			label = Percentage ), size=5) +
			theme_minimal() +
 			theme(legend.position= 'bottom',
				legend.background = element_rect(fill="#CBB7E7",
                                  size=0.5, linetype="solid",
                                  colour ="darkblue")) +
			guides( fill = guide_legend(
				title = "Issue of card",
				title.position = "bottom",
				direction = "horizontal",
				nrows = 2,
				title.theme = element_text(
      				size = 15,
      				face = "italic",
      				colour = "black",
      				angle = 0)
			)) +
			labs(
				x = '',
				y = 'Number of frauds',
				title = sprintf('Total fraud %s', Format(AntalDT[, SUM(antal)]))
			)


pdf(file = file.path(.GRAF, 'share_between_issue_trx_country.pdf'), width=14, height=8,paper='special')
print(PIE)
dev.off()
rm(PIE, AntalDT)

terminalDT	<- fraudDT[, .(Antal = .N), by = .(id_method, entrymode)]
terminalDT	<- merge(terminalDT, entrymodeDT, by.x = 'entrymode', by.y = 'id', all.x = 'TRUE')
terminalDT	<- merge(terminalDT, idmethodDT , by.x = 'id_method', by.y = 'id', all.x = 'TRUE')
terminalDT[is.na(desc.y), desc.y := 'Unknown']

terminalDT[, proportion := round(Antal/SUM(Antal), 2)]

heatMap	<- ggplot(terminalDT, aes(x = desc.y, y = desc.x, fill = proportion))+
 			geom_tile(color = "white")+
			scale_fill_gradient2(low = "blue", high = "red", mid = "white",
   				midpoint = 0.5, limit = c(0,1), space = "Lab", name="Percentage") +
  			theme_minimal()+ # minimal theme
 			theme(axis.text.x = element_text(angle = 45, vjust = 1,
    					size = 12, hjust = 1),
				legend.position= 'bottom',
				legend.background = element_rect(fill="#CBB7E7",
                                  size=0.5, linetype="solid",
                                  colour ="darkblue"))+
 			coord_fixed() +
			geom_text(aes(x = desc.y, y = desc.x, label = Antal), color = "white", size = 4) +
			guides( fill = guide_legend(
				title = "Percentage of fraud",
				title.position = "bottom",
				direction = "horizontal",
				nrows = 2,
				title.theme = element_text(
      				size = 15,
      				face = "italic",
      				colour = "black",
      				angle = 0)
			)) +
			labs(
				x = 'Entry mode',
				y = 'Identification method',
				title = sprintf('Total fraud %s', Format(terminalDT[, SUM(Antal)]))
			)

fraud_by_hour	<- fraudDT[, .(antal = .N), by = .(trx_hour)]
NoFraud		<- ggplot(ems_fraudDT, aes(x = hour, y = antal )) +
    				coord_polar(theta = "x", start = -.13) +
				facet_wrap( ~ type_of_tran)+
    				geom_bar(stat = "identity", fill = "maroon4", width = .9) +
    				geom_hline(yintercept = seq(0, 500, by = 100), color = "grey80", size = 0.3) +
    				scale_x_continuous(breaks = 0:24, expand = c(.002,0)) +
				labs(x = '', y = '',
					title = sprintf(
					'Number of frauds total(%s), based on trx time',
					fraud_by_hour[, Format(SUM(antal))])
				) +

pdf(file = file.path(.GRAF, 'entry_id_method.pdf'), width=14, height=8,paper='special')
print(heatMap)
dev.off()
rm(terminalDT, heatMap)

ems_fraudDT		<- fraudDT[, .(antal = .N) , by = .(trx_day_hour, trx_day_local, is_econ)]
ems_fraudDT[, ':=' (
	hour = hour(trx_day_hour)
)]
ems_fraudDT[, Facet := {
	step1 = SUM( antal )
	MIN_ = min(trx_day_local)
	Max_ = max(trx_day_local)
	defintion = ifelse( is_econ == 1, 'ECON', 'POS')
	paste0(defintion, ' nr trx (', Format(step1), '), daterange ', MIN_ , ' ', Max_)
},  by = .(is_econ)]

Total 	<- ems_fraudDT[, SUM(antal)]

pdf(file = file.path(.GRAF, 'entry_id_method.pdf'), width=14, height=8,paper='special')
print(heatMap)
dev.off()
rm(terminalDT, heatMap)

ems_fraudDT		<- fraudDT[, .(antal = .N) , by = .(date_time_local,date_local)]
ems_fraudDT[, ':=' (
	hour = hour(date_time_local)
)]
ems_fraudDT[, Facet := {
	step1 = SUM( antal )
	MIN_ = min(date_time_local)
	Max_ = max(date_time_local)
	paste0( ' nr trx (', Format(step1), '), daterange ', MIN_ , ' ', Max_)
}]

Total 	<- ems_fraudDT[, SUM(antal)]

Average 		<- ems_fraudDT[, as.integer(MEAN(antal)), by = .(Facet)]
LineChart		<- ggplot(ems_fraudDT, aes(x = date_time_local, y = antal, group = 1)) +
				geom_line( ) +
				facet_grid(	~ Facet ,
					scales = 'free') +
			scale_y_continuous(breaks = pretty_breaks(10), labels = Format) +
			scale_x_datetime(date_breaks = '2 weeks', date_labels = "%Y\n%m\n%d") +
			labs(
				x = 'transaction date',
				y = 'number of transaction per hour',
				title = sprintf('Total number of frauds (%s)', Format(Total))
			) +
			theme_minimal()

pdf(file = file.path(.GRAF, 'fraud_perhour.pdf'), width=12, height=8,paper='special')
plot(LineChart)
dev.off()
rm(ems_fraudDT, Total, LineChart, Average)


fraud_by_hour	<- fraudDT[date_local < '2019-10-01', .(antal = .N), by = .(date_time_local)]
fraud_by_hour[, ':=' (
	trx_day_hour = ymd_hms(date_time_local),
	hour = hour(date_time_local)
)]
fraud_by_hour[, Seq := cut2(hour, c(0,8,16,23))]
fraud_by_hour[, Facet := {
	tmp = round(MEAN(antal),1)
	paste0('Average of frauds per hour ', tmp, ' hours range ', Seq)
}, by = .(Seq)]

LineChart		<- ggplot(fraud_by_hour, aes(x = trx_day_hour, y =antal, group = 1)) +
				geom_point( ) +
				geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) +
				facet_wrap( ~Facet, scales = 'free') +
			scale_x_datetime(date_breaks = '2 months', date_labels = "%Y-%m") +
			labs(
				x = 'transaction date time',
				y = 'number of frauds per hour',
				title = sprintf('Average number of frauds (%s) per hour', as.integer(fraud_by_hour[, MEAN(antal)]))
			) +
			theme_minimal()


pdf(file = file.path(.GRAF, 'fraud_perhour_point_chart.pdf'), width=12, height=8,paper='special')
plot(LineChart)
dev.off()

NoFraud		<- ggplot(ems_fraudDT, aes(x = hour, y = antal )) +
    				coord_polar(theta = "x", start = -.13) +
				#facet_wrap( ~ type_of_tran)+
    				geom_bar(stat = "identity", fill = "maroon4", width = .9) +
    				geom_hline(yintercept = seq(0, 500, by = 100), color = "grey80", size = 0.3) +
    				scale_x_continuous(breaks = 0:24, expand = c(.002,0)) +
				labs(x = '', y = '',
					title = sprintf(
					'Number of frauds total(%s), based on trx time',
					fraud_by_hour[, Format(SUM(antal))])
				) +
    				theme_minimal() +
    				theme(panel.grid.minor = element_blank(),
	  			axis.text.y = element_blank()  )
pdf(file = file.path(.GRAF, 'fraud_perhour.pdf'), width=10, height=8,paper='special')
plot(NoFraud)
#grid.arrange(Fraud, NoFraud)
dev.off()
#############################################################
# Selection based on fraud and mcc
#############################################################
exists('mccDT') || load(file = file.path(.FRAUD, 'mccDT.RData'))
mccTotalDT		<- mccDT[, .(antal = SUM(antal)), by = .(transaction_country , mcc, dim_date, is_fraud )]
mccTotalDT[is.na(is_fraud), is_fraud := 0L]
mccTotalDT[, is_fraud := factor(is_fraud, levels = c(0,1), labels = c('No', 'Yes'))]
mccTotalDT_1		<- dcast.data.table(mccTotalDT, dim_date + mcc + transaction_country ~ is_fraud)
mccTotalDT_1[is.na(Yes), Yes := 0L]
mccTotalDT_1[, All := No + Yes]

mccTotalDT_1[order(dim_date, Yes, decreasing = TRUE), ':=' (
	fraction_fraud = cumsum(Yes)/SUM(Yes),
	fraction_trx = cumsum(All)/SUM(All)
), by = .(dim_date)]

mccTotalDT_1[, ':=' (
	rank_fraud = as.integer(frank(fraction_fraud)),
	rank_trx = as.integer(frank(fraction_trx))
)
, by = .( dim_date)]


idx	<- mccTotalDT_1[, sort(unique(rank_fraud))]
summaryDT	<- data.table()
for( i in idx){
	cat("Run rank ", i, "\n")
	y = 1:i
	dataDT	<- mccTotalDT_1[rank_fraud %in% y,][order(dim_date, rank_fraud)]
	summaryDT	<- rbind(summaryDT,
		dataDT[, .(
			Nr_trx = SUM(All),
			Nr_fraud = SUM(Yes),
			fraction_fraud = mean(fraction_fraud),
			variation_frac_fraud = sd(fraction_fraud),
			fraction_trx = mean(fraction_trx),
			rank_fraud = rank_fraud[.N],
			Nr_mcc = uniqueN(mcc),
			Nr_contries =  uniqueN(transaction_country)
		)]
	)
}

summaryDT[, ':=' (
	diff_trx = fraction_fraud - lag(fraction_fraud),
	diff_fraud = pmax(variation_frac_fraud - lag(variation_frac_fraud),0)
)]


Seq_by <- seq(1, NROW(summaryDT), 1)
graphDt	<- summaryDT[(Seq_by %% 5 == 0 | Seq_by == 1) ][diff_fraud > 0, .(
	`Nr of trx` = Format(Nr_trx),
	`Nr of fraud` = Format(Nr_fraud),
	`Fraud share` = fraction_fraud,
 	Rank = rank_fraud
)]


mccTotalDT	<- merge(
		mccTotalDT_1[rank_fraud < 40, .(
			nr_subset_trx = SUM(All),
			nr_subset_fraud =  SUM(Yes)
		), by = .(dim_date)],
		mccTotalDT_1[, .(
			nr_trx = SUM(All),
			nr_fraud =  SUM(Yes)
		), by = .(dim_date)], by = 'dim_date', all = TRUE
)
mccTotalDT[, ':=' (
	subset_share_trx = round(nr_subset_trx/nr_trx,2),
	subset_share_fraud = round(nr_subset_fraud/nr_fraud,2)
) ]
mccTotalDT	<- rbind(mccTotalDT,
	mccTotalDT[, .(
		nr_subset_trx = SUM(nr_subset_trx),
		nr_subset_fraud = SUM(nr_subset_fraud),
		nr_trx = SUM(nr_trx),
		nr_fraud = SUM(nr_fraud)
	)], fill = TRUE
)
mccTotalDT[.N, ':=' (
	dim_date = 'Total',
	subset_share_fraud = round(nr_subset_fraud/nr_fraud,2),
	subset_share_trx = round(nr_subset_trx/nr_trx,2)
)]

t = mccTotalDT[.N]
save(t, ratioDT_1, file = file.path(.FRAUD, 'summary_report.RData'))

pdf(file = file.path(.GRAF, 'subset_data.pdf'), width=8, height=3,paper='special')
	tableFun(
	 mccTotalDT[, .(
		`Auth month` = dim_date ,
		`Subset trx` = Format(nr_subset_trx),
		`All trx` = Format(nr_trx),
		`Share trx` = subset_share_trx,
		`Subset fraud` = Format(nr_subset_fraud),
		`All fraud` = Format(nr_fraud),
		`Share fraud` = subset_share_fraud
		)]

, plot = TRUE)
dev.off()
rm(mccTotalDT)



Line		<- ggplot(summaryDT, aes(x = as.integer(rank_fraud), diff_fraud )) +
			geom_line(size = 0.9, colour = 'blue') +
			geom_vline(
				xintercept = summaryDT[diff_fraud == 0, .SD[1,  rank_fraud]],
				colour = 'red', size = 0.9
			)+
			coord_cartesian(xlim = c(0,100)) +
  			scale_y_continuous(breaks = pretty_breaks(10), labels = percent) +
			scale_x_continuous(breaks = pretty_breaks(20), limits = c(0, 100), expand = c(0.01, 0)) +
 			annotation_custom(tableGrob(graphDt, rows=NULL),
                    	xmin=60, xmax=65, ymin=0.01, ymax=0.02)+
			labs(x = "Rank on fraud", y = "Marginal share of fraud",
				colour = "", shape = "",
				title = 'Marginal utility of adding another mcc (red line max)') +
			theme_dark()



pdf(file = file.path(.GRAF, 'selection_of_main_dataset.pdf'),
	width=12, height=8,
	paper='special'
)
print(Line)
dev.off()

rm(graphDt, Seq_by, Line, idx); gc(reset = TRUE)
#################################
# Create dummy variable
# entry_mode ->  Unspecified; data unavailable: 0 as reference variable
# idmethodDT -> Other systematic verification: S as reference
# termcap -> Unknown; data not available: 0 as reference
load(file = file.path(.FRAUD, 'subset.RData'))
NR_LOOPS	<- sort(fraud_subset[!is.na(sub_sample), unique(sub_sample)])



summaryDT	<- mainDT[,  .(
	MerchantID = source_merchant_id,
	is_fraud,
	zamount,
 	trx_hour,
	manual_entry = ifelse(entrymode == 1, 1, 0),
	magnetic_stripe_entry = ifelse(entrymode == 2, 1,0 ),
	pan_auto_entry = 	ifelse(entrymode == 5, 1,0 ),
	key_entered_entry = ifelse(entrymode == 6, 1,0 ),
	credential_file_entry = ifelse(entrymode == 7, 1,0 ),
	pan_auto_entry_magnetic_entry = ifelse(entrymode == 'A', 1,0 ),
	magnetic_stripe_pan_entry = ifelse(entrymode == 'B', 1,0 ),
	online_chip_entry = ifelse(entrymode == 'C', 1,0 ),
	offline_chip_entry = ifelse(entrymode == 'F', 1,0 ),
	pan_auto_contancless_entry = ifelse(entrymode == 'M', 1,0 ),
	pan_auto_ecommerce_entry = ifelse(entrymode == 'R', 1,0 ),
	electronic_entry = ifelse(entrymode == 'S', 1,0 ),
	pan_server_entry = ifelse(entrymode == 'T', 1,0 ),
	signature_pos_idmethod = ifelse(id_method== 1, 1,0 ),
	pin_pos_idmethod = ifelse(id_method== 2, 1,0 ),
	unantended_pos_idmethod  = ifelse(id_method== 3, 1,0 ),
	moto_idmethod = ifelse(id_method== 4, 1,0 ),
	three_d_secure_idmethod = ifelse(id_method== 5, 1,0 ),
	seven_ssl_3dsecure_idmethod = ifelse(id_method== 6, 1,0 ),
	seven_ssl_idmethod = ifelse(id_method== 7, 1,0 ),
	non_secure_internet_idmethod = ifelse(id_method== 8, 1,0 ),
	unknown_id_method = ifelse(id_method== 9, 1,0 ),
	manual_no_terminal_termcap = ifelse(term_cap %in% c('1', '2', '4', '5', '6'), 1, 0),
	pan_auto_magnetic_entry_termcap =  ifelse(term_cap == 'A', 1, 0),
	magnetic_stripe_reader_termcap =  ifelse(term_cap == 'B', 1, 0),
	magnetic_stripe_reader_icc_key_termcap =  ifelse(term_cap == 'C', 1, 0),
	magnetic_stripe_reader_icc_termcap =  ifelse(term_cap == 'D', 1, 0),
	icc_key_termcap =  ifelse(term_cap == 'E', 1, 0),
	pan_auto_magnetic_contactless_termcap = ifelse(term_cap == 'M', 1, 0),
	other_termcap = ifelse(term_cap == 'V', 1, 0),
	delta_issue_country = ifelse(card_country != transaction_country, 1,0 )
)]


)

summaryDT[, MerchantID := NULL]

make_factor		<- setdiff(names(summaryDT),
				c('zamount', 'trx_hour', 'Data_Quality_Score__c', 'Rating')
)
make_factor		<- names(
	which(
		sapply(summaryDT[,make_factor	, with = FALSE ], class) == 'factor')
)
summaryDT[, (make_factor) := lapply(.SD, as.character), .SDcols = make_factor]
save(summaryDT ,file = file.path(.FRAUD, 'main_data.RDATA'))
load( file.path(.FRAUD, 'main_data.RDATA'))
summaryDT [, Data_Quality_Score__c := as.numeric(Data_Quality_Score__c)]
summaryDT [, is_fraud := as.factor(is_fraud)]
##################################################################################################
# Machine learning
##################################################################################################
invlogit <- function( x ) {
  step1 <- 1/(1+exp( - x ))
  return( round( step1, 7))
}
get_dataset_oversampled <- function(data, p)
{
	mainTrain.over 	<- ovun.sample(
		is_fraud ~ .,
		p = p,
		data = data,
		method = 'over'
	)
	mainTrain.overDT 	<- mainTrain.over$data
	setDT(mainTrain.overDT)
	return(mainTrain.overDT)
}

predict_wrapper	<- function(x, name, test_dataset)
{
	pred_class 		<- rxPredict(
		x,
		test_dataset,
		type = 'prob',
		extraVarsToWrite = "is_fraud"
	)
	setDT(pred_class)
	setnames(
		pred_class,
		1:2,
		paste0(name, '_pred_', 0:1)
	)
	return(pred_class)
}
library(rpart.plot)
library(ROSE)
library(rpart.plot)
library(pROC)
library(vip)
ColNames	<- names(mainDT)
fraud_subset[, zamount := scale(amount_sek), by = .(source_merchant_id)]
summaryMetric	<- list()
for( .strata in NR_LOOPS)
{
	mainDT1	<- copy(mainDT)
	.sample	<- paste0('sample_', .strata)
	mainDT1	<- rbind(mainDT1,
		fraud_subset[sub_sample == .strata, ][, ColNames, with = FALSE]
	)
	cat(strrep('#', 40), '\n')
	cat("Sample ", .sample, '\n')
	mainDT1	<- merge(mainDT1,
			salesDT[, .(
				MerchantID,
				normal_diligence = ifelse(grepl('DD - Due Diligence', Due_Diligence_Level__c), 1,0),
				Data_Quality_Score__c,
				Rating
			)], by.x = 'source_merchant_id',
			by.y = 'MerchantID', all.x = TRUE)


	summaryDT	<- mainDT1[,.(
		is_fraud = as.factor(is_fraud),
		Data_Quality_Score__c = as.numeric( Data_Quality_Score__c),
		zamount = as.numeric(zamount),
		entrymode = factor(entrymode),
		id_method  = factor(id_method),
		term_cap = factor(term_cap),
		normal_diligence = factor(normal_diligence),
		Rating = as.numeric(ifelse(is.na(Rating), 0, Rating)),
		trx_hour = as.factor(trx_hour),
		delta_issue_country = as.factor(ifelse(card_country != transaction_country, 1,0 ))
	)]
	trainIndex 	<- createDataPartition(summaryDT$is_fraud, p = 0.8, list = FALSE, times = 1)
	mainTrain	<- summaryDT[trainIndex ]
	mainTest	<- summaryDT[!trainIndex ]
	totalTime <- system.time({

	for( prop in rev(seq(0.05, 0.3, by = 0.05)))
	{
		cat(strrep("#", 40), '\n')
		cat("Running proportion ", prop, "\n")
		mainTest_copy	<- copy(mainTest)

		if(prop == 0){
			mainTrain.overDT	<- mainTrain
		} else {
			mainTrain.overDT	<- get_dataset_oversampled(data=mainTrain, p = prop)
		}

		over_sampled <- prop
		over_sampled_key	<- paste0('up_', over_sampled*100)

		X_vars	<- setdiff( names(mainTrain.overDT ), 'is_fraud')
		formula	<- as.formula(paste0('is_fraud ~ ', paste0(X_vars, collapse = '+')))

		fraud_tree	<- rxDTree(formula,
			data = mainTrain.overDT,
			maxDepth=20,
			cp = 0.001,
			minSplit = 10,
			findSplitsInParallel = TRUE,
			xVal = 0
		)
		fraud_dtree1	<- prune.rxDTree(fraud_tree, cp = 0.01)


		fraud_forest_tree	<- rxDForest(formula,
			data = mainTrain.overDT,
			importance = TRUE,
			cp = 0.001,
			findSplitsInParallel = TRUE
		)

		pred_class 		<- predict_wrapper(x = fraud_dtree1, name = 'pruned', mainTest_copy)
		mainTest_copy	<- cbind(mainTest_copy, pred_class[, .(pruned_pred_0, pruned_pred_1)] )
		rm(pred_class)
		pred_class 		<- predict_wrapper(x = fraud_forest_tree, name = 'random', mainTest_copy)
		mainTest_copy	<- cbind(mainTest_copy, pred_class[, .(random_pred_0, random_pred_1)] )


		roc_forest	<- roc(
			is_fraud ~ random_pred_1,
			levels = c(0,1),
			data = mainTest_copy
		)

		roc_pruned	<- roc(
			c(
				substring(x, 1, regexpr('_', x) - 1),
				gsub('.*_([0-9]+).*', '\\1', x),
				sample_list[[x]]$byClass,
				sample_list[[x]]$overall[c('Accuracy')]
			)}
			)
		))
	setDT(tmp)
	tmp[, sample := .sample]
	confusionDT	<- rbind(confusionDT, tmp)
	tmp		<- lapply(.confusion, function(x) {
				step1	<- as.data.table(sample_list[[x]]$table)
				step1[, ':=' (
					algorithm = substring(x, 1, regexpr('_', x) - 1),
					oversample = gsub('.*_([0-9]+).*', '\\1', x)
				)]
			}
	)
	tmp	<- rbindlist(tmp)
	tmp[, sample := .sample]

	confusionTable	<- rbind(confusionTable,  tmp)

	rm(tmp)
	auc_metric	<- as.data.frame(
		do.call(rbind,
		lapply(.roc, function(x) {
			c(
			substring(x, 1, regexpr('_', x) - 1),
			gsub('.*_([0-9]+).*', '\\1', x),
			as.numeric(auc(sample_list[[x]]))
			)}
			)

		)
	)
	setDT(auc_metric)
	auc_metric[, sample := .sample]
	setnames(auc_metric, 1:3, c('algorithm', 'oversample_size', 'auc'))

	aucDT		<- rbind(aucDT, auc_metric)
}

confusionTable[ , Abbr := ifelse(Prediction == 1 & Reference == 1, 'TP',
	ifelse(Prediction == 0 & Reference == 0, 'TN',
 	ifelse(Prediction == 1 & Reference == 0, 'FP', 'FN'
		)
	)
) ]
confusionTable[algorithm == 'pruned' & oversample == 30 ]
confusionTable[ ,
	.(	FPR = .SD[Abbr == 'FP', N]/SUM(N),
		FraudRate = .SD[Abbr %in% c('FP', 'TP'), SUM(N)]/SUM(N)
), by = .(sample, oversample ,algorithm  )]


##    cyl gear  n  N su


save(aucDT, confusionDT, confusionTable, file = file.path(.FRAUD, 'roc_Res.RData'))
confusionDT		<- confusionDT[, lapply(.SD, as.character)]
con_names		<- setdiff(names(confusionDT), c('V1', 'V2', 'sample'))
confusionDT[, (con_names) := lapply(.SD, as.numeric), .SDcols = con_names]
confusionDT[, (con_names) := lapply(.SD,  function(x) round(x,4)), .SDcols = con_names]
tbl <-  tableFun( confusionDT[V2 == 15, .(
		Algorithm = ifelse(V1 == 'random', 'RandomForest', 'Cart'),
		Sample = sample,
		`Balanced Accuracy`,
		Accuracy,
		Sensitivity,
	 	Specificity,
		Precision,
		Recall,
		`Detection Prevalence`,
		F1
	)][order(Algorithm ),])


aucDT[, ':=' (
	Algorithm = ifelse(algorithm== 'pruned', 'Cart', 'RandomForest'),
	Oversample = as.integer(as.character(oversample_size)),
	auc = as.numeric(as.character(auc))
)]


plt =	ggplot(aucDT, aes(x = Oversample , y = auc, colour = Algorithm )) +
		geom_line(size = 1.2) +
		geom_point(size = 3.1) +
		facet_wrap(~sample, scales = 'fixed') +
		theme_minimal() +
		#coord_cartesian(ylim = c(0.8, 1)) +
		scale_x_continuous(breaks = pretty_breaks(10, h = 5)) +
  		scale_y_continuous(breaks = pretty_breaks(10), labels = percent) +
		labs(title = 'Oversample and its effect on auc', x = 'Oversample of fraud') +
		theme(legend.position= 'bottom',
			legend.background = element_rect(fill="#CBB7E7",
                         	size=0.5, linetype="solid",
                                  colour ="darkblue"))+
		guides( colour = guide_legend(
				title = "Algorithm used",
				title.position = "bottom",
				direction = "horizontal",
				nrows = 2,
				title.theme = element_text(
      				size = 15,
      				face = "italic",
      				colour = "black",
      				angle = 0)
			))



pdf(file = file.path(.GRAF, 'baseline_model.pdf'), width=12, height=10,paper='special')
grid.arrange(plt, tbl,
             nrow=2,
             as.table=TRUE,
             heights=c(3,1))
dev.off()
#tree_loss_matrix  <- rpart(is_fraud~ ., method = "class", data= mainTrain, control = rpart.control(cp = 0.0001, minsplit = 10))

















rxVarImpPlot(fraud_forest_tree)
par(layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE),
   widths=c(3,1), heights=c(1,2)))
par(mfrow=c(3,1))
rpart.plot(rxAddInheritance(fraud_dtree1),  main = "Decision tree",
		extra = 106, # display prob of survival and percent of obs
 		fallen.leaves = TRUE,
		nn = TRUE, # display the node numbers
		branch.lty = 3, branch = .5, faclen = 0,
		split.cex = 1.2,
		split.round = .5,
		split.box.col = "lightgray",
		split.border.col = "darkgray"
	)
	plot(roc0, legacy.axes = TRUE, asp=NA,
		print.thres = c(.5), type = "S",
     		print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
		print.thres.cex = .8,
		print.auc=TRUE,
		print.auc.pattern="AUC %.2f"
	)

	barplot(as.rpart(fraud_tree)$variable.importance,
	main = 'Variable importance',
	horiz=FALSE
	)


cp = 0.0001, minsplit = 10
plot(tree_loss_matrix )
text(tree_loss_matrix)
xDTreeBestCp(fraud_tree)
https://blog.revolutionanalytics.com/2013/06/plotting-classification-and-regression-trees-with-plotrpart.html
mod	<- rxLogit(is_fraud ~ zamount + manual_entry + electronic_entry + delta_issue_country   , data = mainTrain)
Va


invlogit( -10.43178 +  0.08510*3 + 2.54140 + 2.59533 + 1.69049)
#############################################################
# mcc description
mcc_file	<- list.files(.DATA, pattern = 'mcc.*xls', full.names = TRUE)
mcc_step1	<- readxl::read_excel(mcc_file, col_names = TRUE, skip = 1)
setDT(mcc_step1)
mccDescription	<- mcc_step1[, .(mcc = `MCC CODE`, mcc_description = `Program Type:`)]
rm(mcc_file, mcc_step1)

if( exists('fraudDT')){
	mccDT		<- unique(fraudDT, by = 'mcc')[, .(mcc,  mcc_description)]
	setkey(mccDT, mcc)
} else {
	load(file.path(.DATA, 'fraud.RData' ))
	mccDT		<- unique(fraudDT, by = 'mcc')[, .(mcc,  mcc_description)]
	setkey(mccDT, mcc)
}


#############################################################
# Selection based on fraud and mcc
#############################################################
# emsAmount		<- emsDT[, .(amount = SUM(amount_sek)), by = .(is_fraud, mcc)]
emsAmount[, Total := SUM(amount), by = .(is_fraud, dim_date)]
emsAmount	<- merge(emsAmount,  mccDescription, by = 'mcc', all.x = TRUE)
emsAmount[, rank_v :=frank(-amount), by = .(is_fraud, dim_date)]

emsCumulative	<-  emsAmount[order(is_fraud, rank_v ),
				 .( summation = amount/Total) , by = .(is_fraud, dim_date, rank_v, mcc_description, mcc)]
emsCumulative[, cumulative := cumsum(summation), by = .(is_fraud, dim_date)]
xhline_cum		<- emsCumulative[cumulative %between% c(0.9,0.91) ][, .SD[rank_v == min(rank_v)], by = .(dim_date)]
idx	<- emsCumulative[is_fraud == 1 & cumulative <= 0.9, unique(mcc)]
setkey(emsDT, mcc)
#############################################################
# Reduce the size of the emsD
#
emsDT		<- emsDT[J(idx)]

Line	<- ggplot(emsCumulative, aes(rank_v,  y = cumulative , group =  is_fraud, colour = as.factor(is_fraud))) +
			geom_line(size = 0.9)  +
			scale_y_continuous(breaks = pretty_breaks(10), labels = percent) +
			scale_x_continuous(breaks = pretty_breaks(10)) +
			coord_cartesian(xlim = c(0,50)) +
			geom_hline(yintercept = 0.9, color = 'blue', size = 1.1) +
			geom_vline(data = xhline_cum, aes(xintercept = rank_v), color = 'blue', size = 1.1) +
			facet_wrap(~ dim_date) +
			theme_minimal() +
 			theme(legend.position= 'bottom',
				legend.background = element_rect(fill="#CBB7E7",
                                  size=0.5, linetype="solid",
                                  colour ="darkblue"))+
			guides( colour = guide_legend(
				title = "Not fraud/Fraud",
				title.position = "bottom",
				direction = "horizontal",
				nrows = 2,
				title.theme = element_text(
      				size = 15,
      				face = "italic",
      				colour = "black",
      				angle = 0)
			)) +
			labs(
				x = 'Ranks',
				y = 'Cumulative distribution',
				title = 'Fraud vs all transactions'
			)

pdf(file = file.path(.GRAF, 'cumulative_distribution.pdf'), width=14, height=8,paper='special')
print(Line)
dev.off()

rm(Line, idx, emsCumulative, xhline_cum	); gc(reset = TRUE)

idx	<- emsAmount[is_fraud == 1 & rank_v %in% 1:20, unique(mcc)]
setkey(emsAmount, mcc)
emsRank	<- emsAmount[J(idx)]
emsRank[, Facet := ifelse(is_fraud == 1, 'Fraud_rank', 'Trx_rank')]
emsRank_1	<- dcast.data.table(emsRank, mcc_description ~  Facet , value.var = c('rank_v'))

#############################################################
# Amount of fraud vs non-fraud
#################
emsRank	<- emsRank[order(is_fraud , rank_v), ]
emsRank$mcc_description <- factor(
			emsRank$mcc_description,
			levels = emsRank[order(is_fraud , rank_v, decreasing = TRUE), unique(mcc_description)]
)
emsRank[, Facet := paste0(ifelse(is_fraud == 1, 'Fraud', 'Non-fraud'), ' turnover ', Format(Total), ' sek')]


BarChart		<- ggplot(emsRank, aes(x = mcc_description, y = amount)) +
				geom_bar(
					position = 'dodge',
					stat = 'identity',
					fill = 'blue',
 					colour = 'black'
				) +
				facet_wrap( ~ Facet , scales = 'free_x') +
				scale_y_continuous(breaks = pretty_breaks(5), labels = Format) +
				coord_flip() +
				theme_minimal() +
				labs(x = '', title = 'Top 20 mcc by fraud and non-fraud') +
				theme(axis.text.y = element_text(face = 'bold'))


pdf(file = file.path(.GRAF, 'top_20_mcc_fraud.pdf'), width=14, height=8,paper='special')
print(BarChart)
dev.off()
rm(emsAmount, topemsAmount, topDT, characters, BarChart)
gc(reset = TRUE)
#################################################################################
# Parse files and save it as csv
#################################################################################



pdf(file = file.path(.GRAF, 'rank_distribution.pdf'), width=6, height=6,paper='special')
	tableFun(emsRank_1[order(Fraud_rank)], plot = TRUE)
dev.off()
rm(emsRank_1, idx, emsAmount, emsRank); gc(reset = TRUE)

#############################################################
# How many fraud by hour and month
#############################################################

file_info	<- data.table(
	full_path = downloaded_file,
	SizeMB = as.integer(file.size(downloaded_file)/(1024^2)),
	keys_date =  search_and_replace('(2019|2018)', basename(downloaded_file))
)[grepl(file_date, keys_date)]

file_info[, cumulative_mb := cumsum(SizeMB )]
MAX_MB	<- file_info[, max(cumulative_mb)]

file_info	<- file_info[order(keys_date, SizeMB)]
file_info[, Seq := cut2(cumulative_mb, seq(0, MAX_MB, 5000)) ]
file_info[, bins := substring(Seq , regexpr('[0-9]', Seq ), (regexpr(',', Seq ) - 1))]


interval_estimation	<- file_info[, .(nr_of_files = .N), by = .(bins)]
interval_estimation[, nr := .I]
setkey(file_info, bins)

OUTPUT_DIR	<- paste0('F:\\ClearingData', file_date)
dir.exists(OUTPUT_DIR) || dir.create(OUTPUT_DIR, recursive = TRUE)
for( nr_of in order(interval_estimation[, nr])[10:NROW(interval_estimation)])
{
	cat(strrep("#", 100), '\n')
	.info		<- interval_estimation[nr == nr_of ]
	.bin		<- .info[, bins]

	processDT		<- file_info[J(.bin), .(full_path, keys_date, SizeMB)]
	.date			<- gsub('-', '_', paste0(processDT[, range(keys_date)], collapse = '_'))
	.size 		<- processDT[, SUM(SizeMB)]
	process_files	<- processDT[, full_path]
	cat('Running file-date and bins ', .date, ' ', .bin, '\n')


for( col_ in names(timeDT))
{
	cat(strrep('#', 60), '\n')
	cat('Running ', col_, '\n')
	acf_data	<- acf(na.omit(timeDT[, col_]), plot = FALSE)
	pacf_data	<- pacf(na.omit(timeDT[, col_]), plot = FALSE)
	pdf(file = file.path(.GRAF, sprintf('lag_correlation_%s.pdf', col_)),
			width = 10,
			height = 8,
			paper = 'special'
	)
	par(mfrow=c(2,1))
	plot(acf_data, main = sprintf('Correlation plot with ACF for (%s)', col_))
	plot(pacf_data, main = sprintf('Correlation plot with ACF for (%s)', col_))
	dev.off()
	fit	<- forecast::tbats(na.omit(timeDT[, col_]))
	p	<- autoplot(fit) +
		  	 scale_y_continuous(labels = Format) +
			labs(title = sprintf('Decomposition using BATS (%s)', col_)) +
			theme_minimal()
	pdf(file = file.path(.GRAF, sprintf('decomp_of_%s.pdf', col_)),
			width = 10,
			height = 8,
			paper = 'special'
	)
	rm(dataset, dataDT)
	gc(reset = TRUE)
}
rm(acf_data, pacf_dat, p); gc(reset = TRUE)


#############################################################
# Amount of fraud based on hour
#############################################################
emsFraud	<- emsDT[, .(antal = .N), by = .(is_fraud, trx_hour )]

Fraud		<- ggplot(emsFraud[is_fraud == 1], aes(x = trx_hour, y = antal )) +
    				coord_polar(theta = "x", start = -.13) +
    				geom_bar(stat = "identity", fill = "maroon4", width = .9) +
    				geom_hline(yintercept = seq(0, 500, by = 100), color = "grey80", size = 0.3) +
    				scale_x_continuous(breaks = 0:24, expand = c(.002,0)) +
    				labs(x = '', y = '',
					title = sprintf(
					'Number of Frauds (%s) segmented by hour during %s',
					emsFraud[is_fraud == 1, Format(SUM(antal))],  dim_date)
				) +
    				theme_minimal() +
    				theme(panel.grid.minor = element_blank(),
	  			axis.text.y = element_blank()  )

NoFraud		<- ggplot(emsFraud[is_fraud == 0], aes(x = trx_hour, y = antal )) +
    				coord_polar(theta = "x", start = -.13) +
    				geom_bar(stat = "identity", fill = "maroon4", width = .9) +
    				geom_hline(yintercept = seq(0, 500, by = 100), color = "grey80", size = 0.3) +
    				scale_x_continuous(breaks = 0:24, expand = c(.002,0)) +
				labs(x = '', y = '',
					title = sprintf(
					'Number of transaction (%s) segmented by hour during %s',
					emsFraud[is_fraud == 0, Format(SUM(antal))],  dim_date)
				) +

    				theme_minimal() +
    				theme(panel.grid.minor = element_blank(),
	  			axis.text.y = element_blank()  )


pdf(file = file.path(.GRAF, 'fraud_perhour.pdf'), width=10, height=8,paper='special')
grid.arrange(Fraud, NoFraud)
dev.off()

rm(emsFraud, NoFraud, Fraud)
gc(reset = TRUE)
#############################################################
# trx amount is very skewed
#############################################################
quantile_amount 	<- emsDT[, quantile(amount_sek, probs = seq(0,1,0.001))]
threshold 		<-  quantile_amount[names(quantile_amount) == '99.0%']


hist	<- ggplot(emsDT[amount_sek < threshold] , aes(amount_sek) )+
		geom_histogram(bins=100, alpha = 0.4, fill=I("blue"))  +
 		facet_wrap(~  is_fraud_text, scales = 'free')  +
		labs(
			x = "Transaction amount (SEK)",
			y = "Distribution",
			title = "99 percent of the transactions"
		) +
		scale_y_continuous(breaks = pretty_breaks(6), labels = Format) +
		theme_minimal()


pdf(file = file.path(.GRAF, 'amount_hist.pdf'), width=10, height=8,paper='special')
print(hist)
dev.off()

add_diffs <- function(DT, newcol, dcol, ndiff){     DT[, (newcol) := get(dcol) - shift(get(dcol), type = "lag", n = ndiff)]     }
setorderv(emsDT, txn_date_time )
setkey(emsDT, merchant_id)


