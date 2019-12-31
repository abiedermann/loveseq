#' Given a list of mixed gene names and GQ accesssions, return a list with
#' accessions translated to PAS wherever possible.
#' @param this.names: list containing mix of gene names and GQ accessions
#' @return PAS translation of the original list
get_PAS_from_mixed <- function(this.names,
                            genome.annot.file='2019Annotation.csv'){
  annot <- read.table(genome.annot.file,header=TRUE,sep=',',quote="\"",row.names=1,stringsAsFactors=FALSE)

  gene.to.gq <- get_genes_to_gq_map()
  for(i in 1:length(this.names)){
    this.gene <- this.names[[i]]

    # skip convertion if gene is already in PAS format
    if(this.gene %in% annot$PAS_names){
      next
    }
    # Handling non-GQ gene names
    if(this.gene %in% names(gene.to.gq)){
      this.gene <- gene.to.gq[this.gene]
    }
    if(nchar(as.character(annot['PAS_names'][as.character(this.gene),])) > 0){
      this.names[[i]] <- annot['PAS_names'][as.character(this.gene),]
    }
  }
  return(this.names)
}
