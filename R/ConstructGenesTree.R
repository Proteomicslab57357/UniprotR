#' Connect and parse UniProt information.
#'
#' This Function is used to plot Genes Tree in the data of the accession/s.
#'
#' @usage ConstructGenesTree(ProteinDataObject,directorypath = NULL)
#'
#' @param ProteinDataObject input a Dataframe returned from GetNamesTaxa function
#'
#' @param directorypath path to save txt file containig results returened by the function.
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
ConstructGenesTree <- function(ProteinDataObject,directorypath = NULL)
{
  ProteinDataObject <- ProteinDataObject %>% select(3)
  ProteinDataObject <- na.omit(ProteinDataObject)
  UniqueLocis <- unique(ProteinDataObject$Gene.names...primary..)
  #Add parent Node
  ChromoTree <- Node$new("GeneNames")
  for(loc in UniqueLocis)
  {
    loca <- ChromoTree$AddChild(loc);
    Frequencyindecies <- which(ProteinDataObject$Gene.names...primary.. %in% loc)
    for (index in Frequencyindecies)
    {
      locaa <- loca$AddChild(rownames(ProteinDataObject)[index])
    }
  }
  if(!is.null(directorypath))
  {
    write.table(ChromoTree , paste0(directorypath,"//","GenesTree.txt"), row.names = F)
  }
  print(ChromoTree)
  plot(ChromoTree)
}
