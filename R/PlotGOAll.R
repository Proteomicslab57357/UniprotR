#' Connect and parse UniProt information.
#'
#' This Function is used to plot the retrieved Gene Ontology from function 'GetProteinGOInfo'.
#'
#' @usage PlotGOAll(GOObj, Top = 10, directorypath = NULL, width = width, height = height)
#'
#' @param GOObj  Dataframe returned from UniprotR Function "GetProteinGOInfo"
#'
#' @param Top Number of molecular functions to be visualized
#'
#' @param directorypath path to save Output plot.
#' 
#' @param width width of the generated plot
#' 
#' @param height height of the generated plot 
#' 
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
#'
PlotGOAll <- function(GOObj, Top = 10, directorypath = NULL, width = width, height = height)
{
  BiologicalDF <- Goparse(GOObj, 3)
  if (dim(BiologicalDF)[1] < 10)
    Top <- dim(BiologicalDF)[1]
  BiologicalDF <- BiologicalDF[1:Top, ]
  BiologicalDF <- na.omit(BiologicalDF)
  BiologicalDF$source <- "BP"
  
  CellularDF <- Goparse(GOObj, 5)
  if (dim(CellularDF)[1] < 10)
    Top <- dim(CellularDF)[1]
  CellularDF <- CellularDF[1:Top, ]
  CellularDF <- na.omit(CellularDF)
  CellularDF$source <- "CC"
  
  
  MolecularDF <- Goparse(GOObj, 4)
  if (dim(MolecularDF)[1] < 10)
    Top <- dim(MolecularDF)[1]
  MolecularDF <- MolecularDF[1:Top, ]
  MolecularDF <- na.omit(MolecularDF)
  MolecularDF$source <- "MF"
  
  GO.terms <- rbind(BiologicalDF, CellularDF)
  GO.terms <- rbind(GO.terms, MolecularDF)
  
  P <- ggbarplot(GO.terms, x = "Goterm", y = "Count",
                 fill = "source",               
                 color = "white",            
                 palette = "jco",            
                 sort.val = "asc",           
                 sort.by.groups = TRUE,      
                 x.text.angle = 90          
  )
  P <- P + scale_fill_lancet() + coord_flip()
  if(!is.null(directorypath))
  {
    ggsave(plot = P, filename = paste0(directorypath, "/", "GO All.jpeg"), device = "jpeg", width = width, height = height)
  }
  plot(P)
}