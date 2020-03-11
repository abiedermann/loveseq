#' Given a list of GQ accesssions, return a list with
#' accessions translated to common gene names wherever possible.
#' @param this.names: list containing only GQ accessions
#' @return mixed translation of the original list (gene names wherever possible
#'         and GQ for everything else
get_mixed_from_GQ <- function(this.names){
  annot <- annotation
  gq.to.gene <- get_gq_to_genes_map()
  for(i in 1:length(this.names)){
    this.gene <- this.names[[i]]

    if(this.gene %in% names(gq.to.gene)){
      this.gene <- as.character(gq.to.gene[this.gene])
      this.names[[i]] <- this.gene
    }
  }
  return(this.names)
}
