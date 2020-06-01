library(usethis)
library(piano)

# Loading most up-to-date version of genome annotation
annotation <- read.table("data-raw/20200108Annotation_plusTFlist.csv",header=T,sep=',',
		row.names=1,quote="\"",stringsAsFactors=F,na.strings="\"\"")

# Pichia kegg genesets and accompanying annotations
kegg.gmt <- loadGSC("data-raw/pichia_kegg_pathways.gmt",type='gmt')
kegg.pathinfo <- read.table("data-raw/kegg_pathway_info.csv",header=TRUE,
			    sep='\t',quote="\"",stringsAsFactors=FALSE)


# Note: Very important for internal=T here. This makes the data visible to
#       functions in the package
usethis::use_data(annotation,kegg.gmt,kegg.pathinfo,overwrite=T,internal=T)
