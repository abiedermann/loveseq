#' Returns a list mapping genes to GQ accessions
get_genes_to_gq_map <- function(annot.filename="2019Annotation.csv"){
  counts <- read.table(annot.filename,header=TRUE,sep=',',quote="\"",row.names=1,stringsAsFactors=FALSE)
  counts['chr'] <- NULL
  counts['strand'] <- NULL

  # renaming rows where FinalSymbol is available
  named.idx <- counts[,'FinalSymbol']!=''
  counts$idx <- rownames(counts)
  gene.map <- data.frame(counts$idx[named.idx],counts[named.idx,'FinalSymbol'])
  colnames(gene.map) <- c("GQ","Gene")
  gene.to.gq <- gene.map$GQ
  names(gene.to.gq) <- gene.map$Gene
  return(gene.to.gq)
}