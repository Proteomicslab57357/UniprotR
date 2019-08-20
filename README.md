# UniprotR : Retrieving Information of Proteins from Uniprot
![Alt text](https://i.ibb.co/jDS7Khq/pinterest-profile-image.png)

# Description
Connect to Uniprot <https://www.uniprot.org/> to retrieve information about proteins using their accession 
number such information could be name or taxonomy information,The package provides a powerful data retrieval capabilities in addition to screening visualization tool, and ID conversion tool that helps researchers for analyzing their proteomics data as well as subsequent downstream analysis.

# Package information
License: GPL-3 <br />
Encoding: UTF-8 <br />
Imports: utils , grDevices , graphics, httr , plyr , scales , stats ,
magrittr , magick , data.tree , ggplot2 <br />
NeedsCompilation: no <br />
Repository: CRAN <br />
link to package on CRAN: https://cran.r-project.org/package=UniprotR <br />

# Example
library(UniprotR) <br />
#Read Accessions from csv file , Note : Accessions must be in the first column <br />
Accessions <- GetAccessionList("https://s3.amazonaws.com/csvpastebin/uploads/9571fa356c67a0c7c95e8431799a051a/Accessions.csv") <br />
#Get Taxanomy Information <br />
TaxaObj <- GetNamesTaxa(Accessions) <br />
#Visualize Chromosomes localization <br />
PlotSummaryInfo(TaxaObj) <br />
#Get Gene ontolgy Information <br />
GeneOntologyObj <- GetProteinGOInfo(Accessions) <br />
#Plot Biological process information <br />
PlotProteinGO_bio(GeneOntologyObj) <br />
#Plot molecular function information <br />
PlotProteinGO_molc(GeneOntologyObj) <br />
#Plot subcellualr localization information <br />
PlotProteinGO_cel(GeneOntologyObj) <br />
#Get Protein-Protein Interaction within input data <br />
GetproteinNetwork(Accessions , Path to save your pdf file) <br />
# Bugs or feature request
To report us of any bugs or new features, please open a new issue or contact email to #Proteomicslab2017@gmail.com.


