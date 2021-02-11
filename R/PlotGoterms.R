#' Connect and parse UniProt information.
#'
#' This Function is used to plot Molecular function of proteins.
#'
#' @usage Plot.GOMolecular(GOObj, Top = 10, directorypath = NULL)
#'
#' @param GOObj  Dataframe returned from UniprotR Function "GetProteinGOInfo"
#'
#' @param Top Number of molecular functions to be visualized
#'
#' @param directorypath path to save Output plot.
#' 
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
#'
Plot.GOMolecular <- function(GOObj, Top = 10, directorypath = NULL)
{
  MolecularDF <- Goparse(GOObj, 4)
  if (dim(MolecularDF)[1] < 10)
    Top <- dim(MolecularDF)[1]
  MolecularDF <- MolecularDF[1:Top, ]
  MolecularDF <- na.omit(MolecularDF)
  MolecularPlot <- ggplot(data = MolecularDF, aes(x = reorder(MolecularDF$Goterm, 
                                                              MolecularDF$Count), y = MolecularDF$Count)) + 
    geom_bar(stat = "identity", fill = "darkgreen") + xlab("Molecular Function") + 
    ylab("Protein count") + theme_bw() + theme(text = element_text(size = 12, 
                                                                   face = "bold", colour = "black"), 
                                               axis.text.x = element_text(vjust = 2)) + coord_flip()
  if(!is.null(directorypath))
  {
    ggsave(plot = MolecularPlot, filename = paste0(directorypath, "/", "Molecular function.jpeg"), device = "jpeg", width = 7, height = 7)
  }
  plot(MolecularPlot)
  return(MolecularPlot)
}

#' Connect and parse UniProt information.
#'
#' This Function is used to plot Biological process of proteins.
#'
#' @usage PlotGOBiological(GOObj, Top = 10, directorypath = NULL)
#'
#' @param GOObj  Dataframe returned from UniprotR Function "GetProteinGOInfo"
#'
#' @param Top Number of molecular functions to be visualized
#'
#' @param directorypath path to save Output plot.
#' 
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
#'
PlotGOBiological <- function(GOObj, Top = 10, directorypath = NULL)
{
  BiologicalDF <- Goparse(GOObj, 3)
  if (dim(BiologicalDF)[1] < 10)
    Top <- dim(BiologicalDF)[1]
  BiologicalDF <- BiologicalDF[1:Top, ]
  BiologicalDF <- na.omit(BiologicalDF)
  BiologicalPlot <- ggplot(data = BiologicalDF, aes(x = reorder(BiologicalDF$Goterm, 
                                                                BiologicalDF$Count), y = BiologicalDF$Count)) + 
    geom_bar(stat = "identity", fill = "darkred") + xlab("Biological Process") + 
    ylab("Protein count") + theme_bw() + theme(text = element_text(size = 12, 
                                                                   face = "bold", colour = "black"),
                                               axis.text.x = element_text(vjust = 2)) + coord_flip()
  if(!is.null(directorypath))
  {
    ggsave(plot = BiologicalPlot, filename = paste0(directorypath, "/", "Biological process.jpeg"), device = "jpeg", width = 7, height = 7)
  }
  plot(BiologicalPlot)
  return(BiologicalPlot)
}

#' Connect and parse UniProt information.
#'
#' This Function is used to plot subcellular localization of proteins.
#'
#' @usage Plot.GOSubCellular(GOObj, Top = 10, directorypath = NULL)
#'
#' @param GOObj  Dataframe returned from UniprotR Function "GetProteinGOInfo"
#'
#' @param Top Number of molecular functions to be visualized
#'
#' @param directorypath path to save Output plot.
#' 
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
#'
Plot.GOSubCellular <- function(GOObj, Top = 10, directorypath = NULL)
{
  CellularDF <- Goparse(GOObj, 5)
  if (dim(CellularDF)[1] < 10)
    Top <- dim(CellularDF)[1]
  CellularDF <- CellularDF[1:Top, ]
  CellularDF <- na.omit(CellularDF)
  CellularPlot <- ggplot(data = CellularDF, aes(x = reorder(CellularDF$Goterm, 
                                                            CellularDF$Count), y = CellularDF$Count)) + 
    geom_bar(stat = "identity", fill = "darkblue") + xlab("Cellular component") + 
    ylab("Protein count") + theme_bw() + theme(text = element_text(size = 12, 
                                                                   face = "bold", colour = "black"),
                                               axis.text.x = element_text(vjust = 2))+ coord_flip()
  if(!is.null(directorypath))
  {
    ggsave(plot = CellularPlot, filename = paste0(directorypath, "/", "Subcellular localization.jpeg"), device = "jpeg", width = 7, height = 7)
  }
  plot(CellularPlot)
  return(CellularPlot)
}