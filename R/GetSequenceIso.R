#' Connect and parse UniProt Sequences information.
#'
#' The function is work to retrieve protein's Sequence data from Uniparc for a list of proteins accessions.
#' This function was added to handle isoformes 
#' 
#' @usage GetSequenceIso(ProteinAccList, directorypath = NULL)
#'
#' @param ProteinAccList Vector of UniProt Accession/s
#'
#' @param directorypath path to save excel file containig results returened by the function
#'
#' @return DataFrame where rows names are the accession
#'      and columns contains the information retrieved from the UniProt
#'
#' @examples Obj <- GetSequenceIso("O14520")
#'
#' @note The function also, Creates a csv file with the retrieved information.
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
GetSequenceIso <- function(ProteinAccList , directorypath = NULL)
{
  if(!has_internet())
  {
    message("Please connect to the internet as the package requires internect connection.")
    return()
  }
  ProteinAccList <- gsub("\\.", "-", ProteinAccList)
  
  BaseUrl <- "https://rest.uniprot.org/uniparc/stream?query="
  ProteinInfoParsed_total = data.frame()
  for (Accession in ProteinAccList){
    RequestURL <- paste0(BaseUrl , Accession ,"&format=fasta")
    Request <- tryCatch(
      {
        GET(RequestURL)
      },error = function(cond)
      {
        message("Internet connection problem occurs and the function will return the original error")
        message(cond)
      }
    )
    if (length(Request) == 0)
    {
      message("Internet connection problem occurs")
      return()
    }
    if (Request$status_code == 200) {
      ProteinDataTable <- tryCatch(read.csv(RequestURL,
                                            header = TRUE), error = function(e) NULL)
      ProteinDataTable1 <- paste(ProteinDataTable[,1], collapse = "")
      ProteinInfoParsed <- as.data.frame(ProteinDataTable1,
                                         row.names = Accession)
      ProteinInfoParsed_total <- rbind(ProteinInfoParsed_total,
                                       ProteinInfoParsed)
    }
  }
  colnames(ProteinInfoParsed_total) <- "Sequence"
  return(ProteinInfoParsed_total)
}
