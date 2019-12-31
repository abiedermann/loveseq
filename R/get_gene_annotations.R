#' Get all relevant info pertaining to genes in a gene list.
#' @param this.list: A list of mixed gene names and GQ accessions
#' @return dataframe containing relevant info for each gene.
get_gene_annotations <- function(this.list,
                                 annot.filename="/Users/biedermann/Dropbox (MIT)/01 Research/01 Experiments/2019Annotation.csv"){
  # Given: A list of mixed gene annotations (AOX1 and GQ annotations)
  # Return: A dataframe containing all relevant annotation info from the master annotation file, with row order identical to
  #         the input list
  this.list.copy <- this.list
  genes_to_gq <- get_genes_to_gq_map()
  idx <- (this.list %in% names(genes_to_gq))
  this.list[idx] <- as.character(genes_to_gq[this.list[idx]])
  annot <- read.table(annot.filename,header=TRUE,sep=',',quote="\"",row.names=1,stringsAsFactors=FALSE)
  annot <- annot[this.list,]
  rownames(annot) <- this.list.copy
  return(annot)
}
