#' Connect and parse UniProt Protein Function information.
#'
#'The function is work to retrieve Protein Function data from UniProt for
#'a list of proteins accessions.For more information about what included in the
#'Protein Function data see https://www.uniprot.org/help/uniprotkb_column_names.
#'
#' @usage GetProteinFunction(ProteinAccList , directorypath = NULL)
#'
#' @param ProteinAccList Vector of UniProt Accession/s
#'
#' @param directorypath path to save excel file containig results returened by the function.
#'
#' @return DataFrame where rows names are the accession
#'      and columns contains the information of protein function roles from the UniProt
#'
#' @note The function also, Creates a csv file with the retrieved information.
#'
#' @examples Obj <- GetProteinFunction("O14520")
#'
#' @export
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}

GetProteinFunction <- function(ProteinAccList , directorypath = NULL)
{
  ProteinInfoParsed_total = data.frame()
  baseUrl <- "http://www.uniprot.org/uniprot/"
  Colnames = "ec,comment(ABSORPTION),comment(CATALYTIC ACTIVITY),chebi,chebi(Catalytic activity),chebi(Cofactor),chebi-id,comment(COFACTOR),comment(ENZYME REGULATION),comment(FUNCTION),comment(KINETICS),comment(PATHWAY),comment(REDOX POTENTIAL),comment(TEMPERATURE DEPENDENCE),comment(PH DEPENDENCE),feature(ACTIVE SITE),feature(BINDING SITE),feature(DNA BINDING),feature(METAL BINDING),feature(NP BIND),feature(SITE)"

  for (ProteinAcc in ProteinAccList)
  {
    #to see if Request == 200 or not
    Request <- GET(paste0(baseUrl , ProteinAcc,".xml"))

    #this link return information in tab formate (format = tab)
    ProteinName_url <- paste0("?query=accession:",ProteinAcc,"&format=tab&columns=",Colnames)
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
    write.csv(ProteinInfoParsed_total , paste0(directorypath , "/","Function roles.csv"))
  }
  return(ProteinInfoParsed_total)
}
