#' Given a matrix of l2cpm data, this function calculates gene-to-gene
#' Pearson correlation co-efficients and returns a matrixs with rows and 
#' columns ordered via hierarchical clustering.
#' @param df: dataframe with rows corresponding to gene names and columns
#'            corresponding to sample names. Values are l2cpm data.
#' @return list(gene.cors,this.hres): correlation data.frame with rows and columns ordered
#'                    via hierarchical clustering. this.hres = hierarchical clustering
#'                    object.
calc_gene_correlation_matrix <- function(df){
    library(fastcluster)
    library(parallelDist)
    # Dropping rows with zero variance
    this.df <- as.matrix(df[which(apply(df,1,var) > 0),])
    # Calculating gene-to-gene correlation matrix
    gene.cors <- cor(t(this.df),t(this.df))
    # Note: by default, parDist used the maximum number of cpu threads available
    this.hres <- fastcluster::hclust(parDist(as.matrix(gene.cors)))
    gene.cors <- gene.cors[this.hres$order,this.hres$order]
    return(list(gene.cors,this.hres))
}

