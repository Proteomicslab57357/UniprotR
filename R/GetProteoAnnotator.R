#' Connect and parse UniProt information.
#'
#' The function is work to retrieve user-defined information data from UniProt
#' for a list of proteins accessions For more
#' information see https://www.uniprot.org/help/uniprotkb_column_names
#'
#' @usage GetProteinAnnontate(ProteinAccList , columns)
#'
#' @param ProteinAccList a vector of UniProt Accession/s
#'
#' @param columns a vector of UniProtKB column names
#'
#'
#' @return DataFrame where rows names are the accession
#'      and columns contains the information retrieved from the UniProt
#'
#' @examples Obj <- GetProteinInteractions("O14520")
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @export


GetProteinAnnontate <- function(ProteinAccList,columns){

  # the core link
  baseUrl <- "http://www.uniprot.org/uniprot/"

  # dataframe to collect columns
  ProteinInfoParsed_total_col = data.frame(x="x")

  for (col in columns)
    {
      #dataframe to collect ProteinAccList
      ProteinInfoParsed_total <- data.frame()


      for (ProteinAcc in ProteinAccList)

      {
        #to see if Request == 200 or not
        Request <- GET(paste0(baseUrl , ProteinAcc,".xml"))

        ProteinName_url <- paste0("?query=accession:",ProteinAcc,"&format=tab&columns=",col)
        RequestUrl <- paste0(baseUrl , ProteinName_url)

        if ( Request$status_code == 200){

          # parse the information in DataFrame

          #if there are information in uniprot then parse_true if not then parse_false
          parse_true <- function()
          {ProteinInfoParsed <- as.data.frame(read.csv(RequestUrl,sep="\t", header=TRUE),row.names = ProteinAcc)
          return(ProteinInfoParsed)
          }
          parse_false <- function()
            {ProteinInfoParsed <- read.csv(RequestUrl,sep="\t", header=TRUE)
            names <- names(ProteinInfoParsed)
            ProteinInfoParsed <- data.frame(name_col = "NA" ,row.names = ProteinAcc)
            colnames(ProteinInfoParsed) <- names
            return(ProteinInfoParsed)}

          ProteinInfoParsed <- tryCatch(parse_true(), error = function(e) parse_false())

          # add Dataframes together if more than one accession
          ProteinInfoParsed_total <- rbind(ProteinInfoParsed_total, ProteinInfoParsed)


          # all Bad requestes
    }else {
      HandleBadRequests(Request$status_code)
    }

      }
      ProteinInfoParsed_total_col <- cbind(ProteinInfoParsed_total_col,ProteinInfoParsed_total)
      remove(ProteinInfoParsed_total)
  }
  ProteinInfoParsed_total_col <- ProteinInfoParsed_total_col[ , !(names(ProteinInfoParsed_total_col) %in% c("x"))]

  return(ProteinInfoParsed_total_col)
}
