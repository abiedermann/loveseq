library(pathview)

#' Given a dataframe with rows corresponding to genes and a column
#' corresponding to sample or stat name, use pathview to visualize
#' pathways in pathid.vector. Genes can be in any format.
#' It is assumed that input.data has already been normalized
#' appropriately for pathview visualization.
#' Output pathview files are written to the subdir file. 
get_kegg_visualizations <- function(input.data,pathid.vector,subdir=".",
                                             excluded.genes=c("Zeocin","P8"),
                                             kegg.pathinfo.filename='/Users/biedermann/Dropbox (MIT)/01 Research/01 Experiments/190718_altcarbon_study/190919_RNA-seq/kegg_pathway_info.csv'){
 
  # Filtering out experiment-specific genes that are not in the annotation file
  input.data = input.data[!(rownames(input.data) %in% excluded.genes),]

  # convert rownames to PAS format
  rownames(input.data) <- get_PAS_from_mixed(rownames(input.data))
  
  # converting from dataframe to a named vector (to fit fgsea input)
  input.data.names <- rownames(input.data)
  input.data <- as.numeric(input.data)
  names(input.data) <- input.data.names
  
  # Save current directory
  base.dir <- getwd()
  
  # Make the required working directory
  dir.create(subdir)
  setwd(subdir)
  # Creating a subdirectory for the other data output by pathview
  dir.create("./kegg_results")
  
  # Pulling KEGG visualizations for specified pathways
  for(i in 1:length(pathid.vector)){
    print(pathid.vector[i])
    pathview(gene.data = input.data, pathway.id = pathid.vector[i], species = "ppa",
             gene.idtype="kegg",low="red",high="green",kegg.dir="./kegg_results")
  }
  
  # return to original working directory
  setwd(base.dir)
}
