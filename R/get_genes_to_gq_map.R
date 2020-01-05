#' Returns a list mapping genes to GQ accessions
get_genes_to_gq_map <- function(){
  counts <- annotation
  # renaming rows where FinalSymbol is available
  named.idx <- counts[,'FinalSymbol']!=''
  counts$idx <- rownames(counts)
  gene.map <- data.frame(counts$idx[named.idx],counts[named.idx,'FinalSymbol'])
  colnames(gene.map) <- c("GQ","Gene")
  gene.to.gq <- gene.map$GQ
  names(gene.to.gq) <- gene.map$Gene
  return(gene.to.gq)
}
