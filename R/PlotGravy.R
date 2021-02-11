#' Connect and parse UniProt information.
#'
#' This Function is used to plot proteins gravy index retrieved from "GetSequences" Function.
#'
#' @usage PlotGravy(SeqDataObjPath , directorypath = NULL)
#'
#' @param SeqDataObjPath Dataframe retrieved from UniprotR Function "GetSequences"
#'
#' @param directorypath path to save Output plot.
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
#' 
PlotGravy <- function(SeqDataObjPath, directorypath = NULL)
{
  seqdata <- select(SeqDataObjPath , "Sequence")
  result = aminoAcidProperties(seqdata , seq="Sequence")
  #Group data frame by GRAVY charge
  GravyGroup <- rep("Negative" , dim(result)[1])
  positive_index <- which(result$Sequence_aa_gravy > 0)
  GravyGroup[positive_index] <- "Positive"
  result <- cbind(result , GravyGroup)
  #Get GRAVY sign ratios
  GRAVYcount <- table(sign(result$Sequence_aa_gravy))
  if (length(GRAVYcount) == 1){
    GRAVYcount <- as.table(cbind(GRAVYcount , 0));
  }
  
  GRAVYratio <- table(sign(result$Sequence_aa_gravy))/dim(result)[1]*100
  if (length(GRAVYratio) == 1){
    GRAVYratio <- as.table(cbind(GRAVYratio , 0));
  }
  #Construct GRAVY dataframe
  GRAVYdf = data.frame(x = c("Negative" , "Positive") , y = GRAVYratio , z = GRAVYcount)
  #GRAVY plot
  GRAVYbarplot <- ggplot(result, aes(x=as.numeric(reorder(rownames(result), result$Sequence_aa_gravy)), y=result$Sequence_aa_gravy, label=rownames(result))) +
    geom_bar(stat='identity', aes(fill=result$GravyGroup)) + theme_classic() + theme(axis.title.x = element_blank() , axis.ticks.x = element_blank() , plot.title = element_text(hjust = 0.5))+ylab("GRAVY index")+
    guides(fill=guide_legend(title="Groups"))+ggtitle("Sequence GRAVY index") + scale_x_continuous(limits = c(0,  dim(result)[1]), expand = c(0, 0))
  
  GRAVYframeplot <- ggplot(GRAVYdf , aes(x = GRAVYdf$x , y = GRAVYdf$z.Freq))+
    geom_bar(stat = "identity" , aes(fill = GRAVYdf$y.Freq)) + geom_text(aes(label = paste0(as.character(round(GRAVYdf$y.Freq),2), "%")) , size = 4, vjust = -0.6)+theme_bw()+
    theme(legend.position = "none" , plot.title = element_text(hjust = 0.5))+xlab("Groups") + ylab("Protein count") + ggtitle("GRAVY index") + scale_y_continuous(limits = c(0,  dim(result)[1]), expand = c(0, 1))
  
  GravyTotal <- ggarrange(GRAVYframeplot, GRAVYbarplot , nrow = 1 , ncol = 2, align = "hv")
  plot(GravyTotal)
  if (!is.null(directorypath))
  {
    ggsave(plot = GravyTotal , filename = paste0(directorypath ,"//" ,"Sequences gravy index.jpeg") , dpi = 320 , width = 11 , height = 10)
    ggsave(plot = GravyTotal , filename = paste0(directorypath ,"//" ,"Sequences gravy index.tiff") , dpi = 320 , width = 11 , height = 10)
  }
}