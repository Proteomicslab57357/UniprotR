#' Connect and parse UniProt Sequences information.
#'
#' The function is work to retrieve Sequence's Length data from Uniparc for a list of proteins accessions.
#' This function was added to overcome the NAs returned when Uniprot database deleted the protein from the database
#' @usage GetSeqLength(ProteinAccList, directorypath = NULL)
#'
#' @param ProteinAccList Vector of UniProt Accession/s
#'
#' @param directorypath path to save excel file containig results returened by the function
#'
#' @return DataFrame where rows names are the accession
#'      and columns contains the information retrieved from the UniProt
#'
#' @examples Obj <- GetSequences("O14520")
#'
#' @note The function also, Creates a csv file with the retrieved information.
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export
GetSeqLength <- function(ProteinAccList , directorypath = NULL)
{
  if(!has_internet())
  {
    message("Please connect to the internet as the package requires internect connection.")
    return()
  }
  BaseUrl <- "https://www.uniprot.org/uniparc/?query="
  ProteinInfoParsed_total = data.frame()
  for (Accession in ProteinAccList){
    RequestURL <- paste0(BaseUrl , Accession ,"&format=tab&force=true&columns=length")
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
                                            header = TRUE, sep = "\t"), error = function(e) NULL)
      ProteinDataTable1 <- ProteinDataTable[1,]
      ProteinInfoParsed <- as.data.frame(ProteinDataTable1,
                                         row.names = Accession)
      ProteinInfoParsed_total <- rbind(ProteinInfoParsed_total,
                                       ProteinInfoParsed)
    }
  }
  colnames(ProteinInfoParsed_total) <- "Length"
  return(ProteinInfoParsed_total)
}
