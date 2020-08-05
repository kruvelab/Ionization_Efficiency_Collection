library(tidyverse)

#*********************
#Script PaDELi jooksutamiseks R-st
#
#KASUTUSJUHEND
#täpsusta PaDEL-Descriptors.jar asukoht muutujasse padel_path
#sisendiks on SMILES-de vektor
#
#TULEMUSEKS dataframe (tibble)
#*********************

padel_batch <- function(padel_path,
                        smiles_vec){
descs <- NULL
for (smiles in smiles_vec){
  file_name_smiles <- "temp.smi"
  file_name_descs <- "output.csv"
  write(smiles,file_name_smiles)
  
  task <- paste('java -jar',
                padel_path,
                '-dir ',
                file_name_smiles,
                '-file ',
                file_name_descs,
                '-2d')
  #Kutsub käsurealt PaDELi välja
  system(task, intern = T)

  descs <- descs %>% 
    bind_rows(file_name_descs %>% 
                read_delim(delim = ",",
                           col_types = cols()))
  file.remove(file_name_smiles)
  file.remove(file_name_descs)
}
descs <- descs %>% 
  mutate(SMILES = smiles_vec) %>% 
  select(-Name)
return(descs)
}
