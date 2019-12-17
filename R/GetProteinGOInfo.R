#' Connect and parse UniProt proteins gene ontology information.
#'
#' The function is work to retrieve proteins gene ontology  data from UniProt
#' for a list of proteins accessions.For more information about what included in the
#' proteins gene ontology data see https://www.uniprot.org/help/uniprotkb_column_names.
#'
#' @usage GetProteinGOInfo(ProteinAccList , directorypath = NULL)
#'
#' @param ProteinAccList Vector of UniProt Accession/s
#'
#' @param directorypath path to save excel file containig results returened by the function.
#'
#' @return DataFrame where rows names are the accession
#'      and columns contains the information of Gene ontology of protein from the UniProt
#'
#' @examples Obj <- GetProteinGOInfo("O14520")
#'
#' @note The function also, Creates a csv file with the retrieved information.
#'
#' @export
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}

GetProteinGOInfo <- function(ProteinAccList , directorypath = NULL)
{
  ProteinInfoParsed_total = data.frame()

  baseUrl <- "http://www.uniprot.org/uniprot/"

  Colnames = "go-id,go,go(biological process),go(molecular function),go(cellular component)"
  for (ProteinAcc in ProteinAccList)
  {
    #to see if Request == 200 or not
    Request <- GET(paste0(baseUrl , ProteinAcc,".xml"))

    #this link return information in tab formate (format = tab)
    #columns = what to return from all of the information (see: https://www.uniprot.org/help/uniprotkb_column_names)
    ProteinName_url <- paste0("?query=accession:",ProteinAcc,"&format=tab&columns=",Colnames)
    RequestUrl <- paste0(baseUrl , ProteinName_url)
    if (Request$status_code == 200){
      # parse the information in DataFrame
      ProteinDataTable <- tryCatch(read.table(RequestUrl, header = TRUE, sep = '\t'), error=function(e) NULL)
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
    write.csv(ProteinInfoParsed_total , paste0(directorypath , "/","Protein GO Info.csv"))
  }
  return(ProteinInfoParsed_total)
}
