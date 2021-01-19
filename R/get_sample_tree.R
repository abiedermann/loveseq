#' Given a matrix of samples and genes, this returns a matrixs with rows and 
#' columns ordered via hierarchical clustering.
#' @param df: dataframe with rows corresponding to gene names and columns
#'            corresponding to sample names. Values are l2cpm data.
#' @return tree: data.tree of clustered samples
get_sample_tree <- function(df){
    library(fastcluster)
    library(parallelDist)
    library(ape)
    library(data.tree)
    tree <- as.Node(as.phylo(fastcluster::hclust(parDist(t(as.matrix(df))))))
    return(tree)
}

