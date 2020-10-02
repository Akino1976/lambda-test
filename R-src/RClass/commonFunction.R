#############################################################################
## Load pkg and script
#############################################################################
## Load pkg and script
library( methods )
library( dplyr )


RowStat		<- function( data, id, 
					type = c('mean', 'var', 'sum','sd', 'min','max')) 
{				
	type    <- match.arg( type )

	FUN     <- switch( type,
                mean = function(x) mean(x, na.rm = TRUE),
                sum	 = function(x) sum(x, na.rm = TRUE),
                var  = function(x) var(x, na.rm = TRUE),
                sd   = function(x) sd(x, na.rm = TRUE),
                min  = function(x) min(x, na.rm = TRUE),
                max  = function(x) max(x, na.rm = TRUE)
            )
            
      Data		<- melt.data.table(data, id.vars = id)
      Data1		<- Data[, FUN(value), by = id]
      Data1		<- merge(data, Data1, by = id, all.x = TRUE)
      setnames(Data1, "V1", "Total")
      return(Data1)
}


tmpSum		<- function(x)
{
	if(is(x,'character') || is(x, 'factor') ||is(x, 'Date'))
	{
		return(NA)
	} else {
		return(SUM(x))
	}
}

#' x Fill in the title of legend
Guide 	<- function(x, ...)
{
	x1 <- guides(	fill = guide_legend(
					  title = x, 
					   title.position = "top", 
					 title.hjust = 0.5,
					title.theme = element_text(size = 10, angle = 0),
					keywidth 	= unit(15, "mm"), 
					keyheight	= unit(4,"mm"),
					label.position = "bottom",
					label.hjust 	= 0.5,
					label.theme 	=  element_text(size = 9, angle = 0),
								... 
					))
	return(x1 )				
}


#' x Fill in the title of legend
GuideCol 	<- function(x, ...)
{
	x1 <- guides(	colour = guide_legend(
					  title = x, 
					   title.position = "top", 
					 title.hjust = 0.5,
					title.theme = element_text(size = 10, angle = 0),
					keywidth 	= unit(15, "mm"), 
					keyheight	= unit(4,"mm"),
					label.position = "bottom",
					label.hjust 	= 0.5,
					label.theme 	=  element_text(size = 9, angle = 0),
								... 
					))
	return(x1 )				
}




char_to_num		<- function(x) as.numeric(as.character(x))


#' USAGE: Object <- new("startUps", pkgs = .PACK, Input = c("data", "graf") )
#' CALL: Object$instant_pkgs( ), will update and install pkgs 
#' CALL: Object$setDirs( Extra ), will create and set path 
#'		to <Input> and if nessecary to the <Extra> character
startUps <- setRefClass("startUps",
			fields 	= list( pkgs = "character", Input = "character", path = "character" ),
			methods	= list(
				instant_pkgs = function( )
				{
					pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
    				if (length(pkgs_miss) > 0)
    				{
        				install.packages(pkgs_miss)
    				}
    
    				if (length( pkgs_miss) == 0)
    				{
        				message("\n ...Packages were already installed!\n")
    				}
   	     			attached <- search()
    				attached_pkgs <- attached[grepl("package", attached)]
    				need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]
    
    				if (length(need_to_attach) > 0)
    				{
        				for (i in 1:length(need_to_attach))
							require(need_to_attach[i], character.only = TRUE)
        }
    
					if (length(need_to_attach) == 0)
					{
        				message("\n ...Packages were already loaded!\n")
					}

				}, # End of function  
				setDirs		= function( )
				{
					if( length(Input) > 0)
					{
						.HOME	<- path
						Input	<<- c(Input)
						if( inherits(Input, "character") )
						{
							Output	<- paste0(toupper(Input), " <- file.path('", .HOME, "','", Input, "')")
						
							for( d in Output)
							{
								cat("************************************************\n")
								String	<- gsub(".*\'(.*)\'.*", "\\1", toupper(d))
								cat("Path for", String , "completed\n")
								Step1 	<- parse(text = d)
								cat("************************************************\n")
								assign(String , eval(Step1), globalenv() ) 
								!file.exists(get( String )) && dir.create( get(String) ,
											 recursive = TRUE)
							} # ForLoop ends here
						} else {
							stop("Need to input character inside ", deparse(substitute(Input)))
						}
					}	
					}	 ## End of function setDirs
			) # End of methodsList	
				
) # End of setRefClass





     
 # USAGE: Will split data|vector into distinct breaks
 # with the <by> options, use ... inside cut
 "Segment" <- function(x, by = 0.15 , ...) {
 	S		<- seq(0,1, by )
 	quantile <- cut(x, breaks = quantile(x, probs = S, na.rm = TRUE), ..., 
        						include.lowest = TRUE, labels =  names(S))
    					
    return ( quantile ) 
}   


pal <- function(col, border = "light gray", ...){
    n <- length(col)
    plot(0,0, type = "n", xlim = c(0,1), ylim = c(0,1),
    axes = FALSE,  xlab = "", ylab = "", ...)
    rect(0:(n-1)/n,0, 1:n/n, 1, col = col, border = border)
}

#' tableFun( data, columCol , fontFace)
#' columCol is only for header and defautl is white text 
#' fontFace is the 1) Plain, 2) bold, 3) italic, 4) both italic and bold
tableFun	<- function( data, columCol =  col_amethyst, plot = FALSE, fontFace = 3)
{
	tt3		<- ttheme_minimal(
		core=list(	bg_params = list(fill='white', col=NA),
					fg_params = list(fontface = fontFace)),
		colhead=list(bg_params = list(fill= columCol, col=NA),
					fg_params = list(fontface = 2, col = "white"))			
	)
	
	step1		<- tableGrob(data, rows = NULL, theme = tt3)
	
	separators 	<- replicate(ncol(step1) - 1,
						segmentsGrob(x1 = unit(0, "npc")),
						simplify = FALSE)
	
	step1	<- gtable::gtable_add_grob( step1, grobs = separators,
							t = 1, b = nrow(step1), l = seq_len(ncol(step1) - 1) + 1)
								
	if( plot ) {
		grid.draw( step1 )
	} else {
		return( step1 )
	}								
												

}


SUM		<- function(x) sum(x, na.rm = TRUE)
MEAN		<- function(x) mean(x, na.rm = TRUE)
LENGTH	<- function(x) length(x, na.rm =TRUE)
FACTOR_INT	<- function(x) as.integer(as.character(x))
#show_col(rgb(140, 212, 237 , maxColorValue = 255 ))

col_bam1 		<- rgb( 69, 42, 86, maxColorValue = 255 )
col_bam2 		<- rgb( 89, 67, 107, maxColorValue = 255 )
col_bam3		<- rgb( 102, 76, 142, maxColorValue = 255 )
col_bam4		<- rgb( 203, 183, 231, maxColorValue = 255 )
col_bam5		<- rgb( 254, 230, 209, maxColorValue = 255 )
col_bam6		<- rgb( 224, 216, 236, maxColorValue = 255 )
col_bam7		<- rgb( 224, 80, 64, maxColorValue = 255 )
col_bam8		<- rgb( 255, 117, 77, maxColorValue = 255 )
col_bam9		<- rgb( 251, 199, 98 , maxColorValue = 255 )
col_bam10		<- rgb( 37, 169, 152 , maxColorValue = 255 )
col_bam11		<- rgb(74, 176, 209 , maxColorValue = 255 )
col_bam12		<- rgb(140, 212, 237 , maxColorValue = 255 )

col_blue		 		<- rgb( 0, 68, 91, maxColorValue = 255 )
col_blueGreen			<- rgb( 89, 155, 161, maxColorValue=255 )
col_green				<- rgb( 44, 171, 102, maxColorValue=255 )
col_red 				<- rgb( 237, 47, 36, maxColorValue=255 )
col_grey		 		<- rgb( 76, 76, 76, maxColorValue=255 )
col_orange		 		<- rgb( 242, 101, 34, maxColorValue=255 )
col_yellow				<- rgb( 251, 173, 29, maxColorValue=255 )
col_neut 				<- rgb( 229, 229, 229, maxColorValue=255 )
col_purple				<- rgb( 147, 112, 229, maxColorValue=255)
col_gold				<- rgb(184, 134, 11, maxColorValue=255 )
col_khaki				<- rgb(189, 183, 107,  maxColorValue=255)
colPro			<- c(sapply( ls(pattern = 'bam'), get),
					"col_blue" = col_blue, "col_blueGreen" = col_blueGreen,
					"col_green" = col_green, "col_red" = col_red,
					"col_orange" = col_orange, "col_khaki" = col_khaki,
					"col_yellow" = col_yellow, "col_purple" = col_purple,
					"col_gold" = col_gold,"col_grey" = col_grey )




