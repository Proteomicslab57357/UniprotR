# UniprotR: Retrieving and visualizing protein sequence and functional information from Universal Protein Resource (UniProtknowledgebase)

[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/UniprotR?color=blue)](https://CRAN.R-project.org/package=UniprotR) 
[![](https://www.r-pkg.org/badges/version/UniprotR?color=green)](https://CRAN.R-project.org/package=UniprotR) 
[![](https://img.shields.io/badge/doi-https%3A%2F%2Fdoi.org%2F10.1016%2Fj.jprot.2019.103613-red)](https://doi.org/10.1016/j.jprot.2019.103613)

![](https://i.ibb.co/jDS7Khq/pinterest-profile-image.png)

# Installation

```R
install.packages("UniprotR")
```

# Description

Connect to [Uniprot](https://www.uniprot.org/) to retrieve information about proteins using their accession 
number such information could be name or taxonomy information,The package provides a powerful data retrieval capabilities in addition to screening visualization tool, and ID conversion tool that helps researchers for analyzing their proteomics data as well as subsequent downstream analysis.

# Documentation

For the documentation see: [UniprotR Documentation](https://cran.r-project.org/web/packages/UniprotR/UniprotR.pdf).

# Package information

- Version: **v1.2.4**
- License: GPL-3
- Encoding: UTF-8
- Imports: utils , grDevices , graphics, httr , plyr , dplyr , scales , stats , magrittr , magick , data.tree , ggplot2 , tidyverse , grid , gridExtra , ggpubr , alakazam
- NeedsCompilation: no
- Repository: CRAN
- link to package on CRAN: [UniprotR](https://cran.r-project.org/package=UniprotR)


# Usage

**Example**

```R
library(UniprotR) 
#Read Accessions from csv file , Note : Accessions must be in the first column. 
Accessions <-GetAccessionList("https://s3.amazonaws.com/csvpastebin/uploads/9571fa356c67a0c7c95e8431799a051a/Accessions.csv") 
#Get Taxanomy Information 
TaxaObj <- GetNamesTaxa(Accessions) 
#Visualize Chromosomes localization
PlotSummaryInfo(TaxaObj)
#Get Gene ontolgy Information 
GeneOntologyObj <- GetProteinGOInfo(Accessions) 
#Plot Biological process information 
PlotProteinGO_bio(GeneOntologyObj) 
#Plot molecular function information 
PlotProteinGO_molc(GeneOntologyObj)
#Plot subcellualr localization information 
PlotProteinGO_cel(GeneOntologyObj) 
#Get Protein-Protein Interaction within input data 
#Path example = "E:/Users/Network.pdf"
GetproteinNetwork(Accessions , Path to save your pdf file) 
```
# Contribution Guidelines

For bugs and suggestions, the most effective way is by raising an issue on the github issue tracker. Github allows you to classify your issues so that we know if it is a bug report, feature request or feedback to the authors.

**Email: Proteomicslab2017@gmail.com**

# Future plans

- Add pathway analysis workflow that connects to KEGG and reactome.
- Launch a a web serivce for UniprotR to facilitate user system interaction
# Citation

M. Soudy, A. Anwar, E.A. Ahmed, et al., UniprotR: Retrieving and visualizing protein sequence and functional information from Universal Protein Resource (UniProt knowledgebase), Journal of Proteomics (2019), https://doi.org/10.1016/j.jprot.2019.103613

![](https://github.com/Proteomicslab57357/UniprotR/blob/master/logos/CCHF_57357.png)
