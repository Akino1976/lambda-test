#!/usr/bin/env Rscript
## load tables
options(scipen=999)
# change later
FULLPATH		<- "/Users/serdarakin/codes/lambda-test/R-SRC"

setwd(FULLPATH)
source(
	list.files(
		pattern = "commonFunction.R",
		recursive = TRUE,
		full.names = TRUE
	)
)
if( file.exists(".Rprofile") ) { source(".Rprofile") }

R_files 	<- list.files(
	path = 'RClass' ,
	pattern = 'commonFunction.R',
	recursive = TRUE,
	full.names = TRUE,
	ignore.case = TRUE
)
source(R_files)
.pkg 		<- c(
	'data.table','aws.s3', 'ndjson', 'optparse', 'readxl', 'stringi', 'DBI',
	'parallel', 'aws.signature','grid', 'Hmisc', 'uuid', 'ggpmisc', 'caret',
	 'bit64', 'scales', 'gridExtra', 'lubridate', 'ggplot2', 'RPostgreSQL'
)
Object 	<- new("startUps", pkgs = .pkg , Input = c("data", "graf") )

Object$instant_pkgs( )

drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "doktor_test",
                 host = "localhost", port = 5432,
                 user = "postgres", password = 'Test-password')

df_postgres <- dbGetQuery(con, "SELECT * FROM view_conversation")


queueDT			 <- dbGetQuery(con, 'SELECT * FROM public.queue_process')
setDT(queueDT)
conversationDT	 <- dbGetQuery(con, 'SELECT * FROM public.view_conversation')
setDT(conversationDT)

data_files 		<- list.files(path = file.path(dirname(FULLPATH), 'DATA'), full.names = TRUE)
library(circular)
conversationDT	<- fread(grep('conversation-export', data_files, value = TRUE))
conversationDT[, ':=' (
	conversation_opened_dt = as.POSIXct(conversation_opened, "%Y-%m-%d %H:%M:%S", tz = "Europe/Stockholm"),
	conversation_closed_dt = as.POSIXct(conversation_closed, "%Y-%m-%d %H:%M:%S", tz = "Europe/Stockholm")
)]
conversationDT[, ':=' (
	con_open_hms = strftime(conversation_opened_dt, format = '%H:%M:%S'),
	con_closed_hms = strftime(conversation_closed_dt, format = '%H:%M:%S')
)]
conversationDT[,  ':=' (
	con_open_hms_numeric = as.numeric(hms(con_open_hms))/3600,
	con_close_hms_numeric = as.numeric(hms(con_closed_hms))/3600
)]
conversationDT 	<- conversationDT[!is.na(con_close_hms_numeric)]

conversationDT[, ':=' (
	conversation_opened_cs =  circular(con_open_hms_numeric, units = "hours", template = "clock24"),
	conversation_closed_cs =  circular(con_close_hms_numeric, units = "hours", template = "clock24")
)]

estimates		 <- mle.vonmises(conversationDT[, conversation_opened_cs])
p_mean 		 <- estimates$mu %% 24
concentration	 <- estimates$kappa
conversationDT[, densities := dvonmises(conversation_opened_cs, mu = p_mean, kappa = concentration)]
top_5			 <- conversationDT[, .N, by = .(category)][order(N, decreasing = TRUE)][1:5, category]


	ggplot(conversationDT[category %in% top_5], aes(x = conversation_closed_cs))+
	facet_wrap(.~ category) +
 	geom_histogram(breaks = seq(0, 24), colour = "blue", fill = "lightblue")  + 
 	coord_polar() +
 	scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24)) +
 	labs(title = sprintf('Top 5 category conversation opened, total: [%s], Date: [%s]', date_frame[, .N], time_frame))

profileDT		<- fread(grep('profile-expor', data_files, value = TRUE))
conversationDT 	<- merge(conversationDT,
		profileDT[, .(
			child_male = SUM(gender == 'm'),
			child_female = SUM(gender == 'f'),
			total_child = .N		
	), by = .(patient_id = user)], by = 'patient_id', all.x = TRUE
)
rm(profileDT)
userDT			<- fread(grep('user-expor', data_files, value = TRUE))
conversationDT 	<- merge(conversationDT,
		userDT[, .(patient_id=id, gender,  age_bin, city)], by = 'patient_id', all.x = TRUE
)

queueDT			<- fread(grep('queue-expor', data_files, value = TRUE))

for(i in names(conversationDT)) set(conversationDT, i=which(is.na(conversationDT[[i]])), j=i, value=NA)
for(i in names(conversationDT)) set(conversationDT, i)
conversationDT	

conversationDT[grepl('midwife|nurse', staff_1_role), cost_estimate := 'lower']
conversationDT[is.na(cost_estimate),  cost_estimate := 'high']

conversationDT[cost_estimate == 'lower' & is.na(staff_2_role), y_variable := 1L ]
conversationDT[is.na(y_variable), y_variable := 0L] 

library(vip)
library(rpart.plot)
library(pROC)
library(rpart)


mainDT		<- conversationDT[number_of_messages > 0 & staff_1_role %in% c('nurse', 'midwife'), 
	.(	y_variable = as.factor(y_variable),
		mins_to_first_assign,
		category = as.factor(category),
		age_bin = as.factor(age_bin), 
		gender,
		number_of_bot_messages,
		number_of_client_messages,
		number_of_staff_messages
	)]
mainDT[, ':=' (
	is_corona	 		= ifelse(grepl('corona', category), 1L, 0L),
	is_prescription 	= ifelse(grepl('prescription', category), 1L, 0L),
	is_skin 			= ifelse(grepl('skin', category), 1L, 0L),
	is_stomach 			= ifelse(grepl('stomach', category), 1L, 0L),
	is_cough 			= ifelse(grepl('cough', category), 1L, 0L),
	other 				= ifelse(!grepl('corona|prescription|skin|stomach|cough', category), 1L, 0L)
)]
mainDT[, category := NULL]
trainIndex  <- createDataPartition(mainDT$y_variable, p = 0.8, list = FALSE, times = 1)
main_train   <- mainDT[trainIndex ]
main_test    <- mainDT[!trainIndex ]

mainDT[, prop.table(table(y_variable))]
main_train[, prop.table(table(y_variable))]
main_test[, prop.table(table(y_variable))]
rpart.plot(fit,  main = "Decision tree", extra = 106, # display prob of survival and percent of obs
	fallen.leaves = TRUE, nn = TRUE, # display the node numbers
	 branch.lty = 3, branch = .5, faclen = 0, split.cex = 1.2,
split.round = .5,
split.box.col = "lightgray",
split.border.col = "darkgray")
fit <- rpart(y_variable ~ .,
   method="class", data= main_train)
vip(fit)

conversationDT[]
