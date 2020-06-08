#' Connect and parse UniProt database identifiers information.
#'
#' The function is work to convert the UniProtKB AC/ID to any database identifiers
#' available by the UniProtKB.
#' For more information about available database identifiers
#' see https://www.uniprot.org/help/api_idmapping.
#'
#' @usage ConvertID(ProteinAccList , ID_from = "ACC+ID" , ID_to = NULL
#'  , directorypath = NULL)
#'
#' @param ProteinAccList  Vector of UniProt Accession/s
#'
#' @param ID_from string of database identifier abbreviation, from which the Accession/ID will be converted
#'
#' @param ID_to string of database identifier abbreviation, to which the Accession/ID will be converted.
#'              default is all database identifier available in UniProtKB
#'
#' @param directorypath path to save excel file containig results returened by the function.
#'
#' @return DataFrame where column one contains the Accession/ID before conversion
#'      and other columns contains the Accession/ID after conversion
#'
#'
#' @note The function also, Creates a csv file with the retrieved information.
#'
#' @export
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}

ConvertID <- function(ProteinAccList, ID_from = "ACC+ID", ID_to = NULL , directorypath = NULL)

  {

    if (is.null(ID_to)){

      ID_to = c("ACC","ID","UPARC","NF50","NF90","NF100","GENENAME",
                "CRC64","EMBL_ID","EMBL","P_ENTREZGENEID","P_GI","PIR",
                "REFSEQ_NT_ID","P_REFSEQ_AC","PDB_ID","BIOGRID_ID",
                "COMPLEXPORTAL_ID","DIP_ID","STRING_ID","CHEMBL_ID",
                "DRUGBANK_ID","GUIDETOPHARMACOLOGY_ID","SWISSLIPIDS_ID",
                "ALLERGOME_ID","ESTHER_ID","MEROPS_ID","MYCOCLAP_ID",
                "PEROXIBASE_ID","REBASE_ID","TCDB_ID","GLYCONNECT_ID",
                "BIOMUTA_ID","DMDM_ID","WORLD_2DPAGE_ID","PROTEOMICSDB_ID",
                "DNASU_ID","ENSEMBL_ID","ENSEMBL_PRO_ID","ENSEMBL_TRS_ID","ENSEMBLGENOME_ID",
                "ENSEMBLGENOME_PRO_ID","ENSEMBLGENOME_TRS_ID",
                "GENEDB_ID","P_ENTREZGENEID","KEGG_ID","PATRIC_ID",
                "UCSC_ID","VECTORBASE_ID","WBPARASITE_ID",
                "ARACHNOSERVER_ID","ARAPORT_ID","CCDS_ID",
                "CGD","CONOSERVER_ID","DICTYBASE_ID","ECHOBASE_ID",
                "ECOGENE_ID","EUHCVDB_ID","EUPATHDB_ID","FLYBASE_ID",
                "GENECARDS_ID","GENEREVIEWS_ID","H_INVDB_ID",
                "HGNC_ID","HPA_ID","LEGIOLIST_ID","LEPROMA_ID",
                "MAIZEGDB_ID","MGI_ID","MIM_ID","NEXTPROT_ID","ORPHANET_ID",
                "PHARMGKB_ID","POMBASE_ID","PSEUDOCAP_ID","RGD_ID",
                "SGD_ID","TUBERCULIST_ID","VGNC_ID","WORMBASE_ID",
                "WORMBASE_PRO_ID","WORMBASE_TRS_ID","XENBASE_ID",
                "ZFIN_ID","EGGNOG_ID","GENETREE_ID","HOGENOM_ID",
                "KO_ID","OMA_ID","ORTHODB_ID","TREEFAM_ID",
                "BIOCYC_ID","REACTOME_ID","UNIPATHWAY_ID","COLLECTF_ID",
                "DISPROT_ID","CHITARS_ID","GENEWIKI_ID","GENOMERNAI_ID") }



    ID_dataframe_total <- data.frame()

    baseUrl <- "https://www.uniprot.org/uploadlists/?"

    for (ProteinAcc in ProteinAccList){

      ID_total <- data.frame(From_UniProtKB_AC_ID = ProteinAcc)

      for (Ids_to in ID_to)
      {
        #to see if Request == 200 or not
        #this link return information in tab formate (format = tab)
        ProteinName_url <- paste0("query=",ProteinAcc,"&format=tab","&from=",ID_from,"&to=",Ids_to)

        RequestUrl <- paste0(baseUrl , ProteinName_url)
        # parse the information in DataFrame
        
        ProteinDataTable <- tryCatch(
          {
            read.table(RequestUrl, header = TRUE, sep = '\t')$To
          },error = function(cond)
          {
            message("Internet connection problem occurs and the function will return the original error")
            message(cond)
          }
        ) 

        ProteinDataTable <- toString(ProteinDataTable)

        ID_total <- cbind(ID_total,ProteinDataTable)

        names(ID_total)[length(ID_total)] <- paste0("To ", Ids_to)


      }
      ID_dataframe_total <- rbind(ID_dataframe_total,ID_total)}
    if(!is.null(directorypath))
    {
      write.csv(ID_dataframe_total , paste0(directorypath , "/" , "Database identifiers.csv"))
    }
    return(ID_dataframe_total)
}
