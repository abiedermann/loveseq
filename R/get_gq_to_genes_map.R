#' Returns a list mapping gq accessions to gene names (where available).
get_gq_to_genes_map <- function(){
  counts <- annotation	
  # renaming rows where FinalSymbol is available
  named.idx <- counts[,'FinalSymbol']!=''
  counts$idx <- rownames(counts)
  gene.map <- data.frame(counts$idx[named.idx],counts[named.idx,'FinalSymbol'])
  colnames(gene.map) <- c("GQ","Gene")
  gq.to.gene <- gene.map$Gene
  names(gq.to.gene) <- gene.map$GQ
  return(gq.to.gene)
}
