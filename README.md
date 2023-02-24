# Rocket-miR

### Introduction:

Rocket-miR is an accessible, point-and-click R Shiny web application designed to stimulate the development of new miRNA-based antimicrobial drug candidates. The application was inspired by prior research demonstrating that eukaryotic miRNAs are capable of regulating virulence genes and pathways in microbial pathogens ([Koeppen et al., 2021](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8285967/); [Cai et al., 2018](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6442475/); [Liu et al., 2016](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4847146/)).

Rocket-miR relies on the bioinformatics algorithm [IntaRNA](https://github.com/BackofenLab/IntaRNA), which predicts the strength of molecular interactions between RNA molecules. The algorithm was applied to predict the potential for interaction between 630 human miRNAs (the set of mature human miRNAs in the [miRGeneDB](https://mirgenedb.org/) Database) and all proteins in 24 human pathogens - including the [ESKAPE pathogens](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6452778/) and a range of pathogens that commonly afflict individuals with the common genetic illness cystic fibrosis (CF). The full set of pathogens included in the application are listed in Supported_Species.csv, within the 'Application Data' folder of this repository.

In the application, users can predict which miRNAs are best suited in general to target individual species, what miRNAs are most likely to target specific biological pathways, how certain miRNAs are predicted to perform across a group of species, and more. To learn more and try the application for yourself, you can access it via the application link below. 

### Requirements:

You can access the published version of Rocket-miR online at the following link: *Link will be available soon*

To run the R Shiny application on your own computer in Rstudio, download the contents of the repository as a zip file using the green ‘Code’ button at the top right of this web page. 

In order to get the app working, you will first need to set up the application data files (containing miRNA target predictions and gene-pathway annotations for each pathogen). You can do so by working through the ‘Data_Setup.Rmd’ script step-by-step. Once the data is generated, you can run the app.R file in RStudio and the app will launch on your computer. 

### Referencing Rocket-miR:

If you use Rocket-miR to advance your own research, please cite the following publication: 

*Placeholder: Reference will be available as soon as the associated manuscript is submitted…*

If you have any questions about the application or its source code - or would like to apply the Rocket-miR framework to new species and/or new small RNA molecules - please reach out to neff [dot] sam1 [at] gmail [dot] com
