#' Get all relevant info pertaining to genes in a gene list.
#' @param this.list: A list of mixed gene names and GQ accessions
#' @return dataframe containing relevant info for each gene.
get_gene_annotations <- function(this.list){
  # Given: A list of mixed gene annotations (AOX1 and GQ annotations)
  # Return: A dataframe containing all relevant annotation info from the master annotation file, with row order identical to
  #         the input list
  this.list.copy <- this.list
  genes_to_gq <- get_genes_to_gq_map()
  idx <- (this.list %in% names(genes_to_gq))
  this.list[idx] <- as.character(genes_to_gq[this.list[idx]])
  annot <- annotation
  annot <- annot[this.list,]
  rownames(annot) <- this.list.copy
  return(annot)
}
