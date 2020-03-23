#' Connect and parse UniProt information.
#'
#' This function can be used to get a list of UniProt Accession/s from a csv file.
#'
#' @usage PlotPhysicochemical(SeqDataObjPath , directorypath = NULL)
#'
#' @param SeqDataObjPath Dataframe returned from GetSequence function.
#'
#' @param directorypath Path to save Physcochemical properties plot.
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
PlotPhysicochemical <- function(SeqDataObjPath , directorypath = NULL)
{
  seqdata <- select(SeqDataObjPath , "Sequence")
  result = aminoAcidProperties(seqdata , seq="Sequence")

  #Group data frame by charge
  ChargeGroup <- rep("Negative" , dim(result)[1])
  positive_index <- which(result$Sequence_AA_CHARGE > 0)
  ChargeGroup[positive_index] <- "Positive"
  result <- cbind(result , ChargeGroup)

  #Get charge Sign ratios
  Chargecount <- table(sign(result$Sequence_AA_CHARGE))
  if (length(Chargecount) == 1){
    Chargecount <<- as.table(cbind(Chargecount , 0));
  }
  Chargeratio <- table(sign(result$Sequence_AA_CHARGE))/dim(result)[1]*100
  if (length(Chargeratio) == 1){
    Chargeratio <<- as.table(cbind(Chargeratio , 0));
  }
  #Construct Charge dataframe
  Chargedf = data.frame(x = c("Negative" , "Positive") , y = Chargeratio , z = Chargecount)
  #Group data frame by GRAVY charge
  GravyGroup <- rep("Negative" , dim(result)[1])
  positive_index <- which(result$Sequence_AA_GRAVY > 0)
  GravyGroup[positive_index] <- "Positive"
  result <- cbind(result , GravyGroup)
  #Get GRAVY sign ratios
  GRAVYcount <- table(sign(result$Sequence_AA_GRAVY))
  if (length(GRAVYcount) == 1){
    GRAVYcount <- as.table(cbind(GRAVYcount , 0));
  }

  GRAVYratio <-  table(sign(result$Sequence_AA_GRAVY))/dim(result)[1]*100
  if (length(GRAVYratio) == 1){
    GRAVYratio <- as.table(cbind(GRAVYratio , 0));
  }
  #Construct GRAVY dataframe
  GRAVYdf = data.frame(x = c("Negative" , "Positive") , y = GRAVYratio , z = GRAVYcount)

  #Charge plot
  Chargebarplot <- ggplot(result, aes(x=as.numeric(reorder(rownames(result) , result$Sequence_AA_CHARGE)), y=result$Sequence_AA_CHARGE, label=rownames(result))) +
    geom_bar(stat='identity', aes(fill=result$ChargeGroup)) + theme_classic() + theme(axis.title.x = element_blank()  , axis.ticks.x = element_blank() , plot.title = element_text(hjust = 0.5))+ylab("Protein charge")+
    guides(fill=guide_legend(title="Groups"))+ggtitle("Sequence Charge") + scale_x_continuous(limits = c(0,  dim(result)[1]), expand = c(0, 0))
  Chargebarplot

  Chargeframeplot <- ggplot(Chargedf , aes(x = Chargedf$x , y = Chargedf$z.Freq))+
    geom_bar(stat = "identity" , aes(fill = Chargedf$y.Freq)) + geom_text(aes(label = paste0(as.character(round(Chargedf$y.Freq),2), "%")) , size = 4, vjust = -1)+theme_bw()+
    theme(legend.position = "none" , plot.title = element_text(hjust = 0.5))+xlab("Groups") + ylab("Protein count") + ggtitle("Sequence Charge") + scale_y_continuous(limits = c(0,  dim(result)[1]), expand = c(0, 1))

  Chargeframeplot

  #GRAVY plot
  GRAVYbarplot <- ggplot(result, aes(x=as.numeric(reorder(rownames(result), result$Sequence_AA_GRAVY)), y=result$Sequence_AA_GRAVY, label=rownames(result))) +
    geom_bar(stat='identity', aes(fill=result$GravyGroup)) + theme_classic() + theme(axis.title.x = element_blank() , axis.ticks.x = element_blank() , plot.title = element_text(hjust = 0.5))+ylab("GRAVY index")+
    guides(fill=guide_legend(title="Groups"))+ggtitle("Sequence GRAVY index") + scale_x_continuous(limits = c(0,  dim(result)[1]), expand = c(0, 0))
  GRAVYbarplot


  GRAVYframeplot <- ggplot(GRAVYdf , aes(x = GRAVYdf$x , y = GRAVYdf$z.Freq))+
    geom_bar(stat = "identity" , aes(fill = GRAVYdf$y.Freq)) + geom_text(aes(label = paste0(as.character(round(GRAVYdf$y.Freq),2), "%")) , size = 4, vjust = -0.6)+theme_bw()+
    theme(legend.position = "none" , plot.title = element_text(hjust = 0.5))+xlab("Groups") + ylab("Protein count") + ggtitle("GRAVY index") + scale_y_continuous(limits = c(0,  dim(result)[1]), expand = c(0, 1))

  GRAVYframeplot

  Allplot <- ggarrange(Chargebarplot , Chargeframeplot  , GRAVYbarplot , GRAVYframeplot , nrow = 2 , ncol = 2, align = "hv")
  Allplot


  #Plot acidic vs Basic Groups
  AcidicBasic <- c(result$Sequence_AA_ACIDIC , result$Sequence_AA_BASIC)
  AcidicBasicGroup <- rep("Acidic" , length(result$Sequence_AA_ACIDIC))
  AcidicBasicGroup <- c(AcidicBasicGroup , rep("Basic" , length(result$Sequence_AA_BASIC)))
  AcidicBasicframe <- data.frame(x = AcidicBasic , y = AcidicBasicGroup)


  p<-ggplot(AcidicBasicframe, aes(x=AcidicBasicframe$y, y=AcidicBasicframe$x, fill=AcidicBasicframe$y)) +
    geom_violin(alpha = 0.3)+ geom_boxplot(width=0.1) +
    guides(fill=guide_legend(title="Groups"))+ xlab("Groups") + ylab("Hydrophobicity")+
    theme_bw() + ggtitle("Hydrophobicity") + theme(plot.title = element_text(hjust = 0.5))

  p
  TestAll <- ggarrange(Allplot , p , ncol = 2 , nrow = 1)
  plot(TestAll)
  if (!is.null(directorypath))
  {
    ggsave(filename =paste0(directorypath , "//" , "Physochemical.jpeg") , plot = TestAll , device = "jpeg" , width = 15 , height = 8 , dpi = 320)
    ggsave(filename =paste0(directorypath , "//" , "Physochemical.tiff") , plot = TestAll , device = "tiff" , width = 15 , height = 8 , dpi = 320)

  }

}
