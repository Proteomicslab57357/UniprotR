#' Connect and parse UniProt protein Interactions information.
#'
#' The function is work to retrieve protein Interactions data from UniProt for a
#' list of proteins accessions.For more information about what included in the
#' protein Interactions data see https://www.uniprot.org/help/uniprotkb_column_names.
#'
#' @usage GetProteinInteractions(ProteinAccList , directorypath = NULL)
#'
#' @param ProteinAccList Vector of UniProt Accession/s
#'
#' @param directorypath path to save excel file containig results returened by the function.
#'
#' @return DataFrame where rows names are the accession
#'      and columns contains the information of protein Interactions from the UniProt
#'
#' @examples Obj <- GetProteinInteractions("ProteinAccession")
#'
#' @note The function also, Creates a csv file with the retrieved information.
#'
#' @export
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}

GetProteinInteractions <- function(ProteinAccList , directorypath = NULL)
{
  if(!has_internet())
  {
    message("Please connect to the internet as the package requires internect connection.")
    return()
  }
  ProteinInfoParsed_total = data.frame()
  baseUrl <- "http://www.uniprot.org/uniprot/"
  Colnames = "comment(SUBUNIT),interactor"
  for (ProteinAcc in ProteinAccList)
  {
    #to see if Request == 200 or not
    Request <- tryCatch(
      {
        GET(paste0(baseUrl , ProteinAcc,".xml") , timeout(7))
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
    RequestUrl <- URLencode(RequestUrl)
    
    if (Request$status_code == 200){
      # parse the information in DataFrame
      ProteinDataTable <- tryCatch(read.csv(RequestUrl, header = TRUE, sep = '\t'), error=function(e) NULL)
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
  }
  if(!is.null(directorypath))
  {
    write.csv(ProteinInfoParsed_total , paste0(directorypath , "/" , "Protein Interactions.csv"))
  }
  return(ProteinInfoParsed_total)
}
