#' Connect and parse UniProt PTM_Processsing information.
#'
#' The function is work to retrieve PTM_Processsing data from UniProt for a list of proteins accessions.
#' For more information about what included in the PTM_Processsing data
#' see https://www.uniprot.org/help/uniprotkb_column_names.
#'
#' @usage GetPTM_Processing(ProteinAccList, directorypath = NULL)
#'
#' @param ProteinAccList Vector of UniProt Accession/s
#'
#' @param directorypath path to save excel file containig results returened by the function
#'
#' @return DataFrame where rows names are the accession
#'      and columns contains the information retrieved from the UniProt
#'
#' @examples Obj <- GetPTM_Processing("O14520" )
#'
#' @note The function also, Creates a csv file with the retrieved information.
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export


GetPTM_Processing<- function(ProteinAccList, directorypath = NULL ){

  # PTM_Processsing information to be collected
  columns <- c("comment(PTM),feature(CHAIN),feature(CROSS LINK),feature(DISULFIDE BOND),feature(GLYCOSYLATION),feature(INITIATOR METHIONINE),feature(LIPIDATION),feature(MODIFIED RESIDUE),feature(PEPTIDE),feature(PROPEPTIDE),feature(SIGNAL),feature(TRANSIT)")
  baseUrl <- "http://www.uniprot.org/uniprot/"
  ProteinInfoParsed_total = data.frame()
  for (ProteinAcc in ProteinAccList)
  {
    #to see if Request == 200 or not
    Request <- GET(paste0(baseUrl , ProteinAcc,".xml"))

    #this link return information in tab formate (format = tab)
    #columns = what to return from all of the information (see: https://www.uniprot.org/help/uniprotkb_column_names)
    ProteinName_url <- paste0("?query=accession:",ProteinAcc,"&format=tab&columns=",columns)

    RequestUrl <- paste0(baseUrl , ProteinName_url)
    RequestUrl <- URLencode(RequestUrl)
    if (Request$status_code == 200){
      # parse the information in DataFrame
      ProteinDataTable <- tryCatch(read.csv(RequestUrl, header = TRUE, sep = '\t'), error=function(e) NULL)
      if (!is.null(ProteinDataTable))
      {
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
    write.csv(ProteinInfoParsed_total ,paste0(directorypath,"/","PTM_Processsing Information.csv"))
  }
  return(ProteinInfoParsed_total)
}
