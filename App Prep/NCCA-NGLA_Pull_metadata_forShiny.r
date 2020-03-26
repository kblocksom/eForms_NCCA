# NCCA-NGLA_Pull_metadata_forShiny.r

library(RODBC)
library(tidyr)
library(magrittr)

chanNCCA <- odbcConnect('NCCA20')

meta20 <- sqlQuery(chanNCCA,"SELECT TABLE_NAME,SAMPLE_TYPE,PARAMETER,TRAIT,RESULT FROM tblMETADATA WHERE ACTIVE='TRUE' AND PORTION='1' AND TRAIT IN('DESCRIPTION','APP_FIELD_NAME','APP_FORM_NAME','APP_LEGAL_VALUES')", stringsAsFactors=F)

metadata <- filter(meta20, TABLE_NAME!='NOT_IN_DATABASE') %>%
  pivot_wider(id_cols=c(TABLE_NAME,SAMPLE_TYPE,PARAMETER), names_from='TRAIT', values_from='RESULT')

saveRDS(metadata, "c:/users/kblockso/local/eForms_NCCA20/data/metadata.rds")