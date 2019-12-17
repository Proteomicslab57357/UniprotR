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
  GO_df_obj_bio <- toString(na.omit(GOObj[,index]))

  GO_df_obj_bio <- strsplit(GO_df_obj_bio,";|,")

  GO_df_obj_bio_df <- data.frame(GO_df_obj_bio)

  colnames(GO_df_obj_bio_df) <- "bio"


  trim <- function (x) gsub("^\\s+|\\s+$", "", x)

  GO_df_obj_bio_df$bio <- trim(GO_df_obj_bio_df$bio)

  test1 <- strsplit(as.character( GO_df_obj_bio_df$bio ), ".\\[")

  test2 <- lapply(test1, function(x) x[[1]][1])

  occurences<-table(unlist(test2))

  occurences<- as.data.frame(occurences)

  occurences <-  occurences[order(-occurences$Freq),]

  colnames(occurences) <- c("Goterm","Frequences")

  occurences %>%
    mutate(freq = percent(occurences$Freq / length(rownames(GOObj)))) -> occurences
  return(occurences)
}

#' Connect and parse UniProt information.
#'
#' This Function is used to plot data retrieved from UniprotR Function "Goparse".
#'
#' @usage PlotGoInfo(GOObj , directorypath = NULL)
#'
#' @param GOObj Dataframe.
#'
#' @param directorypath path to save excel file containig results returened by the function ( default = NA ).
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
#'
PlotGoInfo <- function(GOObj , directorypath = NULL)
{
  #Get Biologica process Data
  BiologicalDF <- Goparse(GOObj , 3)
  #Plot top 5
  BiologicalDF <- BiologicalDF[1:10,]
  rownames(BiologicalDF) <- c()
  #Plot Biological function
  BiologicalPlot <- ggplot(data=BiologicalDF, aes(x=reorder(BiologicalDF$Goterm , BiologicalDF$Frequences), y=BiologicalDF$Frequences)) +
    geom_bar(stat="identity",fill="darkred") + xlab("Biological GO Info") + ylab("Protein count")+
    theme_bw()+theme(text = element_text(size=12, face="bold", colour="black"),axis.text.x = element_text(vjust=2))
  BiologicalPlot <- BiologicalPlot + coord_flip()
  #Get molecuar function
  MolecularDF <- Goparse(GOObj , 4)
  #Plot top 5
  MolecularDF <- MolecularDF[1:10,]
  rownames(MolecularDF) <- c()
  MolecularPlot <- ggplot(data=MolecularDF, aes(x=reorder(MolecularDF$Goterm , MolecularDF$Frequences), y=MolecularDF$Frequences)) +
    geom_bar(stat="identity",fill="darkgreen") + xlab("Molecular GO Info") + ylab("Protein count")+
    theme_bw()+theme(text = element_text(size=12, face="bold", colour="black"),axis.text.x = element_text(vjust=2))
  MolecularPlot <- MolecularPlot + coord_flip()

  #Get cellular component
  CellularDF <- Goparse(GOObj , 5)
  #Plot top 5
  CellularDF <- CellularDF[1:10,]
  rownames(CellularDF) <- c()
  CellularPlot <- ggplot(data=CellularDF, aes(x=reorder(CellularDF$Goterm , CellularDF$Frequences), y=CellularDF$Frequences)) +
    geom_bar(stat="identity",fill="darkblue") + xlab("Cellular GO Info") + ylab("Protein count")+
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

  GoSummary <- grid.arrange(gtable_combine(tableGrob(BiologicalDF), tableGrob(MolecularDF), tableGrob(CellularDF) , along=2))


  GoInfoPlot <- ggarrange(Goplots , GoSummary , ncol = 2 ,  align = "h")
  if (!is.null(directorypath)){
    ggsave(plot = GoInfoPlot , filename = paste0(directorypath ,"//" ,"GoPlotInfo.jpeg") , device = "jpeg" , dpi = 320 , width = 16 , height = 10)
    ggsave(plot = GoInfoPlot , filename = paste0(directorypath ,"//" ,"GoPlotInfo.tiff") , device = "tiff" , dpi = 320 , width = 16 , height = 10)

  }
}
