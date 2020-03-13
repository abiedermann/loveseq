#' Returns mixture of gene annotations and GQs associated with a specific
#' kegg pathway, by common name (case sensitive).
get_kegg_genes <- function(path.name){
  pas.list <- kegg.gmt$gsc[path.name][[1]]
  pas.to.gq <- get_pas_to_gq_map()
  gene.list <- get_mixed_from_GQ(as.character(pas.to.gq[pas.list]))
  return(gene.list)
}

