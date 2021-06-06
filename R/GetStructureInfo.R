#' Connect and parse UniProt protein Structure information.
#'
#' The function is work to retrieve Structral data from UniProt for a list of proteins accessions.
#' For more information about what included in the structral 
#' data see https://www.uniprot.org/help/uniprotkb_column_names.
#'
#' @usage GetStructureInfo(ProteinAccList, directorypath = NULL)
#'
#' @param ProteinAccList Vector of UniProt Accession/s.
#'
#' @param directorypath path to save excel file containig results returened by the function.
#'
#' @return DataFrame where rows names are the accession
#'      and columns contains the Structural information of protein from the UniProt
#'
#' @note The function also, Creates a csv file with the retrieved information.
#'
#'
#' @export

GetStructureInfo <- function(ProteinAccList, directorypath = NULL)
{
  if(!has_internet())
  {
    message("Please connect to the internet as the package requires internect connection.")
    return()
  }
  ProteinInfoParsed_total = data.frame()
  baseUrl <- "http://www.uniprot.org/uniprot/"
  Colnames = "3d,feature(BETA STRAND),feature(HELIX),feature(TURN)"
  message("Please wait we are processing your accessions ...")
  pb <- progress::progress_bar$new(total = length(ProteinAccList))
  for (ProteinAcc in ProteinAccList)
  {
    #to see if Request == 200 or not
    Request <- tryCatch(
      {
        GET(paste0(baseUrl , ProteinAcc,".xml") , timeout(60))
      },error = function(cond)
      {
        message("Internet connection problem occurs and the function will return the original error")
        message(cond)
      }
    )
    #this link return information in tab formate (format = tab)
    ProteinName_url <- paste0("?query=accession:",ProteinAcc,"&format=tab&columns=",Colnames)
    RequestUrl <- paste0(baseUrl , ProteinName_url)
    if (length(Request) == 0)
    {
      message("Internet connection problem occurs")
      return()
    }
    if (Request$status_code == 200){
      # parse the information in DataFrame
      ProteinDataTable <- tryCatch(read.table(RequestUrl, header = TRUE, sep = '\t'), error=function(e) NULL)
      if (!is.null(ProteinDataTable))
      {
        ProteinDataTable <- ProteinDataTable[1,]
        ProteinInfoParsed <- as.data.frame(ProteinDataTable,row.names = ProteinAcc)
        # add Dataframes together if more than one accession
        ProteinInfoParsed_total <- rbind(ProteinInfoParsed_total, ProteinInfoParsed)
      }
    }else {
      HandleBadRequests(Request$status_code)
    }
    pb$tick()
    
  }
  if (!is.null(directorypath)) {
  write.csv(ProteinInfoParsed_total ,paste0(directorypath, "/" ,"Structral Info.csv"))
  }
  return(ProteinInfoParsed_total)
}
