#' Connect and parse UniProt information.
#'
#' This Function is used to parse data retrieved from UniprotR Function "GetProteinGOInfo".
#'
#' @usage Goparse(GOObj , index = 3)
#'
#' @param GOObj  Dataframe.
#'
#' @param index idex of Go term in GoObj
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
#'
Goparse <- function(GOObj , index = 3)
{
  GO_df_obj_bio <- as.character(na.omit(GOObj[, index]))
  GO_df_obj_bio <- strsplit(GO_df_obj_bio, ";")
  Terms <- rm_between(unlist(GO_df_obj_bio), "[", "]")
  occurences <- table(unlist(Terms))
  occurences <- as.data.frame(occurences)
  occurences <- occurences[order(-occurences$Freq), ]
  colnames(occurences) <- c("Goterm", "Count")
  occurences <- occurences %>% mutate(`%` = percent(occurences$Count/length(rownames(GOObj))))
  return(occurences)
}

#' Connect and parse UniProt information.
#'
#' This Function is used to plot data retrieved from UniprotR Function "GetProteinGOInfo".
#'
#' @usage PlotGoInfo(GOObj , directorypath = NULL)
#'
#' @param GOObj Dataframe retrieved from UniprotR Function "GetProteinGOInfo".
#'
#' @param directorypath path to save excel file containig results returened by the function ( default = NA ).
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
#'
PlotGoInfo <- function(GOObj , directorypath = NULL)
{
  #Get Biological process Data
  BiologicalDF <- Goparse(GOObj , 3)
  #Plot top 10
  BiologicalDF <- BiologicalDF[1:10,]
  BiologicalDF <- na.omit(BiologicalDF)

  #Plot Biological function
  BiologicalPlot <- ggplot(data=BiologicalDF, aes(x=reorder(BiologicalDF$Goterm , BiologicalDF$Count), y=BiologicalDF$Count)) +
    geom_bar(stat="identity",fill="darkred") + xlab("Biological Process") + ylab("Protein count")+
    theme_bw()+theme(text = element_text(size=12, face="bold", colour="black"),axis.text.x = element_text(vjust=2))
  BiologicalPlot <- BiologicalPlot + coord_flip()
  
  
  
  #Get molecular function
  MolecularDF <- Goparse(GOObj , 4)
  #Plot top 10
  MolecularDF <- MolecularDF[1:10,]
  MolecularDF <- na.omit(MolecularDF)
  
  MolecularPlot <- ggplot(data=MolecularDF, aes(x=reorder(MolecularDF$Goterm , MolecularDF$Count), y=MolecularDF$Count)) +
    geom_bar(stat="identity",fill="darkgreen") + xlab("Molecular Function") + ylab("Protein count")+
    theme_bw()+theme(text = element_text(size=12, face="bold", colour="black"),axis.text.x = element_text(vjust=2))
  MolecularPlot <- MolecularPlot + coord_flip()

  #Get cellular component
  CellularDF <- Goparse(GOObj , 5)
  #Plot top 10
  CellularDF <- CellularDF[1:10,]
  CellularDF <- na.omit(CellularDF)
  
  CellularPlot <- ggplot(data=CellularDF, aes(x=reorder(CellularDF$Goterm , CellularDF$Count), y=CellularDF$Count)) +
    geom_bar(stat="identity",fill="darkblue") + xlab("Cellular component") + ylab("Protein count")+
    theme_bw()+theme(text = element_text(size=12, face="bold", colour="black"),axis.text.x = element_text(vjust=2))
  CellularPlot <- CellularPlot + coord_flip()

  #Plot Go Info
  Goplots <- ggarrange(BiologicalPlot , MolecularPlot  , CellularPlot , nrow = 3 , ncol = 1 , align = "hv")

  #arrange tables

  #GoSummary <- grid.arrange(
  #  tableGrob(BiologicalDF),
  #  tableGrob(MolecularDF),
  #  tableGrob(CellularDF),
  #  along = 2,
  #  nrow=3)

  GoSummary <- grid.arrange(gtable_combine(tableGrob(BiologicalDF, rows = NULL),
                                           tableGrob(MolecularDF, rows = NULL),
                                           tableGrob(CellularDF, rows = NULL) , along=2))


  GoInfoPlot <- ggarrange(Goplots , GoSummary , ncol = 2 ,  align = "h")
  if (!is.null(directorypath)){
    ggsave(plot = GoInfoPlot , filename = paste0(directorypath ,"//" ,"GoPlotInfo.jpeg") , dpi = 320 , width = 19 , height = 10)
    ggsave(plot = GoInfoPlot , filename = paste0(directorypath ,"//" ,"GoPlotInfo.tiff") , dpi = 320 , width = 19 , height = 10)

  }
  plot(GoInfoPlot)
}
