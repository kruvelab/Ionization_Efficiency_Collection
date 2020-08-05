library(tidyverse)
library(rcdk)
library(enviPat)

isotope_distribution <- function(smiles){
  formula <- get_formula_from_SMILES(smiles)
  # Chemical formula to isotope distribution
  isotopologues_list <- get_isotopologues(formula)
  isotopologues <- as_tibble(isotopologues_list[[1]])
  isotope_correction <- isotopologues %>% 
    select(abundance) %>% 
    sum()
  return(isotope_correction)
}
isotope_distribution <- Vectorize(isotope_distribution)

get_formula_from_SMILES <- function(SMILES){
  molecule <- parse.smiles(SMILES)[[1]]
  formula <- get.mol2formula(molecule,charge=0)
  formula <- formula@string
  return(formula)
}

get_isotopologues <- function(formula){
  data(isotopes)
  pattern <- isopattern(isotopes,
                        formula,
                        threshold=0.1,
                        plotit=FALSE,
                        charge=FALSE,
                        algo=1)
  return(pattern)
}