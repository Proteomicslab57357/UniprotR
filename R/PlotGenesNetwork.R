#' Connect and parse UniProt information.
#'
#' This Function is used to cluster proteins based on primary genes retrieved from "GetNamesTaxa" Function.
#'
#' @usage PlotGenesNetwork(ProteinDataObject , directorypath = NULL)
#'
#' @param ProteinDataObject Dataframe retrieved from UniprotR Function "GetNamesTaxa"
#'
#' @param directorypath path to save Output plot.
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
#' 
PlotGenesNetwork <- function(ProteinDataObject, directorypath = NULL)
{
  ProteinDataObject <- ProteinDataObject %>% select(3)
  ProteinDataObject <- na.omit(ProteinDataObject)
  UniqueLocis <- unique(ProteinDataObject$Gene.names...primary..)
  ChromoTree <- Node$new("GeneNames")
  for (loc in UniqueLocis) {
    loca <- ChromoTree$AddChild(loc)
    Frequencyindecies <- which(ProteinDataObject$Gene.names...primary.. %in% 
                                 loc)
    for (index in Frequencyindecies) {
      locaa <- loca$AddChild(rownames(ProteinDataObject)[index])
    }
  }
  #plot with networkD3
  useRtreeList <- ToListExplicit(ChromoTree, unname = TRUE)
  Net <- radialNetwork(useRtreeList)
  if(!is.null(directorypath))
  {
    saveWidget(Net, file = paste0(directorypath,"/", "Protein clustering.html"))
  }
  return(Net)
}