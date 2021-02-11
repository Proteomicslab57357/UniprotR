#' Connect and parse UniProt information.
#'
#' This Function is used to get diseases associated with proteins.
#'
#' @usage Get.diseases(Pathology_object , directorypath = NULL)
#'
#' @param Pathology_object Dataframe retrieved from UniprotR Function "GetPathology_Biotech"
#'
#' @param directorypath path to save Output file
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
#' 
Get.diseases <- function(Pathology_object, directorypath = NULL)
{
  Disease <- select(Pathology_object, "Involvement.in.disease")
  Disease <- setNames(data.frame(Disease[!is.na(Disease$Involvement.in.disease),]), "Involvement.in.disease")
  if (dim(Disease)[1] == 0)
  {
    return(NULL)
  }
  Protein.disease <- NULL
  
  Disease.count.list <- setNames(data.frame(unlist(str_extract_all(Disease$Involvement.in.disease ,"DISEASE:(.*?)]"))), "DISEASE")
  Disease.count <- setNames(data.frame(plyr::count(Disease.count.list , "DISEASE")), c("Disease", "Numberofproteins"))
  Disease.count$Disease <- gsub("DISEASE: " , "" , Disease.count$Disease)
  Disease.count <- Disease.count[order(Disease.count$Numberofproteins, decreasing = T),]
  #Get OMIM Ids
  #OMIMID <- do.call('rbind', strsplit(unlist(ex_between(Disease.count$Disease ,"[", "]")), ":"))[,2]
  #Disease.count$OMIMID <- CreateOMIMlink(OMIMID)
  if(!is.null(directorypath))
  {
    write.csv(x = Disease.count, file = paste0(directorypath, "/", "Diseases.csv"))
  }
  return(Disease.count)
}