#' Given a vector of genes, this function returns a vector of genes which matches
#' at least one of the domains entered in the vector of domain queries. Domain
#' queries are assumed to be in raw format. Regex searches are not currently
#' supported.
filter_genes_by_domain <- function(gene.vector,domain.vector){
	library(Hmisc)
	library(dplyr)
	genes_to_gq <- get_genes_to_gq_map()
	idx <- (gene.vector %in% names(genes_to_gq))
  	gene.vector[idx] <- as.character(genes_to_gq[gene.vector[idx]])

	annot <- annotation[gene.vector,]
	annot$Name <- rownames(annot) # dplyr loses rownames...
	print(paste0("Searching ",domain.vector[1],"..."))
	filtered.annot <- annot[grepl(escapeRegex(domain.vector[1]),annot$Domains),]
	if(length(domain.vector)>1){
		for(d in domain.vector[2:length(domain.vector)]){
			print(paste0("Searching ",d,"..."))
			this.filter <- annot[grepl(
				escapeRegex(d),annot$Domains),]
			filtered.annot <- dplyr::union(filtered.annot,this.filter)
		}
	}
	matches <- filtered.annot$Name
	gq_to_genes <- get_gq_to_genes_map()
	idx <- (matches %in% names(gq_to_genes))
        matches[idx] <- as.character(gq_to_genes[matches[idx]])
	return(matches)
}

