% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetproteinNetwork_all.R
\name{GetproteinNetwork_all}
\alias{GetproteinNetwork_all}
\title{Connect and parse stringdb information.}
\usage{
GetproteinNetwork_all(ProteinAccList , directorypath = NULL, SpeciesID = 9606)

GetproteinNetwork_all(ProteinAccList , directorypath = NULL,SpeciesID = 9606)
}
\arguments{
\item{ProteinAccList}{input a vector of UniProt Accession/s}

\item{directorypath}{path to save excel file containig results returened by the function.}

\item{SpeciesID}{Taxonomic id of accession's species ex. homo sapines 9606}
}
\description{
This function is connecting to stringdb and retrieve PPI between input list
}
\author{
Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
}
