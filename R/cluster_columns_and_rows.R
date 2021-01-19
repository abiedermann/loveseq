#' Given a matrix of samples and genes, this returns a matrixs with rows and 
#' columns ordered via hierarchical clustering.
#' @param df: dataframe with rows corresponding to gene names and columns
#'            corresponding to sample names. Values are l2cpm data.
#' @return clust.df: dataframe with rows corresponding to gene names and columns
#'            corresponding to sample names. Values are l2cpm data. Clusted
#'            according to Euclidean distance between samples.
cluster_columns_and_rows <- function(df){
    library(fastcluster)
    library(parallelDist)
    df <- as.matrix(df[which(apply(df,1,var) > 0),])
    this.hres <- fastcluster::hclust(parDist(t(as.matrix(df))))
    this.hres.rows <- fastcluster::hclust(parDist(as.matrix(df)))
    return(df[this.hres.rows$order,this.hres$order])
}

