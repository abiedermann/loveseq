#' Given a matrix of l2cpm data, this function calculates gene-to-gene
#' Pearson correlation co-efficients and returns a matrixs with rows and 
#' columns ordered via hierarchical clustering.
#' @param df: dataframe with rows corresponding to gene names and columns
#'            corresponding to sample names. Values are l2cpm data.
#' @return gene.cors: correlation data.frame with rows and columns ordered
#'                    via hierarchical clustering.
calc_gene_correlation_matrix <- function(df){
    gene.cors <- cor(t(df),t(df))
    # Note: by default, parDist used the maximum number of cpu threads available
    this.hres <- hclust(parDist(gene.cors))
    gene.cors <- gene.cors[this.hres$order,this.hres$order]
    return(gene.cors)
}

