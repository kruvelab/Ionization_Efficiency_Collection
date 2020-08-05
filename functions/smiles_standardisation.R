library(tidyverse)
library(rcdk)
standardize_smiles <- function(smiles_string){
  tryCatch({
    molecule <- parse.smiles(smiles_string)[[1]]
  }, error = function(e){
    print(paste("Parsing failed for:",smiles_string))
    print(e)
    molecule <- NULL
  })
  if (!is.null(molecule)){
    std_smiles <- get.smiles(molecule,
                             flavor = smiles.flavors("Isomeric"))
  } else {
    std_smiles <- NULL
  }
  
  return(std_smiles)
}
standardize_smiles <- Vectorize(standardize_smiles)
