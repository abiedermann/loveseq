#' Given a dataframe with rows corresponding to genes and a column
#' corresponding to sample or stat name, use pathview to visualize
#' pathways involved in protein processing. Genes can be in any format.
#' It is assumed that input.data has already been normalized
#' appropriately for pathview visualization.
#' Output pathview files are written to the subdir file. 
visualize_protein_path <- function(input.data,pathid.vector,subdir=".",
                                             excluded.genes=c("Zeocin","P8")){

  pathid.vector <-      c("04011", # map kinase pathways
                          "04138", # autophagy
                          "03020", # RNA polymerases
                          "03015", # mRNA surveillance pathway
                          "03013", # RNA transport
                          "03008", # Ribosome biogenesis
                          "03010", # Ribosomes
                          "04111", # cell cycle regulation
                          "04141", # ER
                          "04130", # SNARE interactions in excretion)
                          "03050") # Proteasome

  get_kegg_visualizations(input.data,pathid.vector,subdir,excluded.genes)
}
