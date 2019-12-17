#' Connect and parse UniProt information.
#'
#' This Function is used to plot biological process data retrieved from UniprotR Function "GetProteinGOInfo".
#'
#' @usage PlotProteinGO_bio(GO_df , dir_path = NA)
#'
#' @param GO_df  Dataframe.
#'
#' @param dir_path path to save files returened by the function ( default = NA ).
#'
#' @note if no dir_path was given ( default = NA ) the function will only view the plot and will not save it
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
#'

PlotProteinGO_bio <- function(GO_df, dir_path = NA)
{

  GO_df_obj_bio <- toString(GO_df$Gene.ontology..biological.process.)

  GO_df_obj_bio <- strsplit(GO_df_obj_bio,";|,")

  GO_df_obj_bio_df <- data.frame(GO_df_obj_bio)

  colnames(GO_df_obj_bio_df) <- c("bio")

  trim <- function (x) gsub("^\\s+|\\s+$", "", x)

  GO_df_obj_bio_df$bio <- trim(GO_df_obj_bio_df$bio)

  test1 <- strsplit(as.character( GO_df_obj_bio_df$bio ), ".\\[")

  test2 <- lapply(test1, function(x) x[[1]][1])

  occurences<-table(unlist(test2))

  occurences<- as.data.frame(occurences)

  occurences <-  occurences[order(-occurences$Freq),]

  colnames(occurences) <- c("biological_process","Frequences")

  occurences %>%
    mutate(freq = percent(occurences$Freq / sum(occurences$Freq))) -> occurences

  bar_plot <- ggplot(data=occurences, aes(x= reorder(occurences$biological_process ,occurences$Frequences)  , y=occurences$Frequences)) +
    geom_bar(stat="identity", fill="steelblue" , alpha = 0.7) + xlab("Frequency") + ylab("Biological function")+
   # geom_text(aes(label = occurences$freq), vjust = -0.03) + theme(axis.text.x = element_text(angle = 90 , hjust = 1 , vjust = 0.2))+
    theme_minimal() +coord_flip() +theme(text = element_text(size=12))
  print (bar_plot)



  if ( !is.na(dir_path) ) {
  ggsave(paste0(dir_path, "//" ,"GO_plot_bio.jpeg") ,plot = bar_plot , device = "jpeg",dpi = 300,width = 20, height = 20 )
  }

}

#######################

#' Connect and parse UniProt information.
#'
#' This Function is used to plot molecular functions data retrieved from UniprotR Function "GetProteinGOInfo".
#'
#' @usage PlotProteinGO_molc(GO_df , dir_path = NA)
#'
#' @param GO_df  Dataframe.
#'
#' @param dir_path path to save files returened by the function ( default = NA ).
#'
#' @note if no dir_path was given ( default = NA ) the function will only view the plot and will not save it
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
#'
PlotProteinGO_molc <- function(GO_df, dir_path = NA)
{

  GO_df_obj_bio <- toString(GO_df$Gene.ontology..molecular.function.)

  GO_df_obj_bio <- strsplit(GO_df_obj_bio,";|,")

  GO_df_obj_bio_df <- data.frame(GO_df_obj_bio)

  colnames(GO_df_obj_bio_df) <- c("bio")

  trim <- function (x) gsub("^\\s+|\\s+$", "", x)

  GO_df_obj_bio_df$bio <- trim(GO_df_obj_bio_df$bio)

  test1 <- strsplit(as.character( GO_df_obj_bio_df$bio ), ".\\[")

  test2 <- lapply(test1, function(x) x[[1]][1])

  occurences<-table(unlist(test2))

  occurences<- as.data.frame(occurences)

  occurences <-  occurences[order(-occurences$Freq),]

  colnames(occurences) <- c("molecular_functions","Frequences")

  occurences %>%
    mutate(freq = percent(occurences$Freq / sum(occurences$Freq))) -> occurences

  bar_plot <- ggplot(data=occurences, aes(x=reorder(occurences$molecular_functions , occurences$Frequences), y=occurences$Frequences)) +
    geom_bar(stat="identity", fill="steelblue" , alpha = 0.7) + xlab("Frequency") + ylab("molecular function")+
    geom_text(aes(label = occurences$freq), vjust = -0.03) + theme(axis.text.x = element_text(angle = 90 , hjust = 1 , vjust = 0.2))+
    theme_minimal() +coord_flip() + theme_bw()+theme(text = element_text(size=12, face="bold", colour="black"),axis.text.x = element_text(vjust=2))
  print (bar_plot)

  if ( !is.na(dir_path) ) {
    ggsave(paste0(dir_path, "//" ,"GO_plot_molc.jpeg") ,plot = bar_plot , device = "jpeg",dpi = 300,width = 20, height = 15 )
  }

}

#####################


#' Connect and parse UniProt information.
#'
#' This Function is used to plot cellular components data retrieved from UniprotR Function "GetProteinGOInfo".
#'
#' @usage PlotProteinGO_cel(GO_df , dir_path = NA)
#'
#' @param GO_df  Dataframe.
#'
#' @param dir_path path to save files returened by the function ( default = NA ).
#'
#' @note if no dir_path was given ( default = NA ) the function will only view the plot and Will not save it
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
#'
PlotProteinGO_cel <- function(GO_df, dir_path = NA)
{

  GO_df_obj_bio <- toString(GO_df$Gene.ontology..cellular.component.)

  GO_df_obj_bio <- strsplit(GO_df_obj_bio,";|,")

  GO_df_obj_bio_df <- data.frame(GO_df_obj_bio)

  colnames(GO_df_obj_bio_df) <- c("bio")

  trim <- function (x) gsub("^\\s+|\\s+$", "", x)

  GO_df_obj_bio_df$bio <- trim(GO_df_obj_bio_df$bio)

  test1 <- strsplit(as.character( GO_df_obj_bio_df$bio ), ".\\[")

  test2 <- lapply(test1, function(x) x[[1]][1])

  occurences<-table(unlist(test2))

  occurences<- as.data.frame(occurences)

  occurences <-  occurences[order(-occurences$Freq),]

  colnames(occurences) <- c("cellular_components","Frequences")

  occurences %>%
    mutate(freq = percent(occurences$Freq / sum(occurences$Freq))) -> occurences

  bar_plot <- ggplot(data=occurences, aes(x=reorder(occurences$cellular_components, occurences$Frequences), y=occurences$Frequences)) +
    geom_bar(stat="identity", fill="steelblue" , alpha = 0.7) + xlab("Frequency") + ylab("cellular component")+
    geom_text(aes(label = occurences$freq), vjust = -0.03) + theme(axis.text.x = element_text(angle = 90 , hjust = 1 , vjust = 0.2))+
    theme_minimal() +coord_flip() + theme_bw()+theme(text = element_text(size=12, face="bold", colour="black"),axis.text.x = element_text(vjust=2))


  print (bar_plot)

  if ( !is.na(dir_path) ) {
    ggsave(paste0(dir_path, "//" ,"GO_plot_cell.jpeg") ,plot = bar_plot , device = "jpeg",dpi = 300,width = 20, height = 10 )
  }

}
