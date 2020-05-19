#' Connect and parse UniProt information.
#'
#' This Function is used to plot location's Tree in the data of the accession/s in the chromosomes.
#'
#' @usage ConstructLocTree(ProteinDataObject,directorypath = NULL)
#'
#' @param ProteinDataObject input a Dataframe returned from GetNamesTaxa function
#'
#' @param directorypath path to save txt file containing results returened by the function.
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
ConstructLocTree <- function(ProteinDataObject,directorypath = NULL)
{
  ProteinDataObject <- ProteinDataObject %>% select(10)
  ProteinDataObject <- na.omit(ProteinDataObject)
  UniqueLocis <- unique(ProteinDataObject$Proteomes)

  #Add parent Node
  ChromoTree <- Node$new("Chromosomes")
  for(loc in UniqueLocis)
    {
    loca <- ChromoTree$AddChild(loc);
    Frequencyindecies <- which(ProteinDataObject$Proteomes %in% loc)
    for (index in Frequencyindecies)
      {
      locaa <- loca$AddChild(rownames(ProteinDataObject)[index])
    }
    print(ChromoTree)
  }
  if(!is.null(directorypath))
  {
    write.table(ChromoTree , paste0(directorypath,"//","ChromosomesTree.txt"), row.names = F)
  }
  print(ChromoTree)
  plot(ChromoTree)
}
