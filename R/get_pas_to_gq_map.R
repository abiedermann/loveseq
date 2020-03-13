#' Returns a list mapping genes to GQ accessions
get_pas_to_gq_map <- function(){
  counts <- annotation
  # renaming rows where FinalSymbol is available
  named.idx <- counts[,'PAS_names']!=''
  counts$idx <- rownames(counts)
  gene.map <- data.frame(counts$idx[named.idx],counts[named.idx,'PAS_names'])
  colnames(gene.map) <- c("GQ","PAS")
  pas.to.gq <- gene.map$GQ
  names(pas.to.gq) <- gene.map$PAS
  return(pas.to.gq)
}
