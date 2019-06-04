# eFormsOrganizeData_byTable.r
# Purpose: For each type of data, organize into data frames
# First figure out the type of data by sample type
#
# Created 2/27/2019 by Karen Blocksom
###############################################################

eFormsOrganize_byTable <- function(rawData){
  # Extract visit info
  visitinfo <- as.data.frame(rawData[1:7],stringsAsFactors=F)
  # Extract sample type from 8th element in each file
  sampletype <- names(rawData)[8]
  # # PHAB sample types are special
  # sampletype <- ifelse(substring(sampletype,1,5) %in% c('PHABW','PHABB'),substring(sampletype,1,5),
  #                      sampletype)
   
  # Create data frame of parsed data to start with, making them all character variables 
  parsedData <- as.data.frame(rawData[8])
  parsedData[,names(parsedData)] <- lapply(parsedData[,names(parsedData)], as.character)
  
  # run parsed data through organizing function, based on sample type 
  switch(sampletype,
    ASSESSMENT = {rr <- organizeAssessment(parsedData)},
    CALIBRATION = {rr <- organizeCalibration(parsedData)},
    VERIFICATION = {rr = organizeVerification(parsedData)},
    SAMPLES = {rr = organizeSamples(parsedData)},
    PROFILE = {rr = organizeProfile(parsedData)}
  )
  
  ss <- list(cbind(visitinfo, rr))
    # Add new object to list with sample type name
  ss[["SAMPLE_TYPE"]] <- sampletype
  return(ss)
}

#############################################################################################################
# This begins the section which organizes the parsed data by sample type

organizeVerification <- function(parsedIn){
# Simply melt these data and clean up parameter names
  aa <- mutate(parsedIn, SAMPLE_TYPE='VERIF') %>%
    melt(id.vars=c('SAMPLE_TYPE'), variable.name='PARAMETER', value.name='RESULT') %>%
    mutate(PARAMETER=gsub('VERIFICATION\\.', '', PARAMETER)) %>%
    select(SAMPLE_TYPE, PARAMETER, RESULT) %>%
    mutate(SAMPLE_TYPE=ifelse(str_detect(PARAMETER,'MACRO_ALGAE|BOTTOM_TYPE|HABITAT|MACRO_ABUNDANCE|MACROALGAE|MARINE_DEBRIS|MARINE_DEBRIS_TYPE|SAV|SAV_ABUNDANCE'),'SHAB','VERIF'))
  
  return(aa)
}

organizeSamples <- function(parsedIn){
  # Simply melt these data by SAMPLE_TYPE and clean up parameter names
  aa <- mutate(parsedIn, SAMPLE_TYPE='SAMPLES') %>%
    melt(id.vars=c('SAMPLE_TYPE'), value.name='RESULT') %>%
    mutate(SAMPLE_TYPE=substring(as.character(variable),9,12), 
           PARAMETER=ifelse(str_detect(variable,'\\_COMMENT'),substring(as.character(variable),9,nchar(as.character(variable))),
                            substring(as.character(variable),14,nchar(as.character(variable))))) %>%
    select(SAMPLE_TYPE, PARAMETER, RESULT)
  
  return(aa)
}

organizeAssessment <- function(parsedIn){
  
  # Simply melt data and clean up parameter names
  aa <- mutate(parsedIn, SAMPLE_TYPE='ASSESS') %>%
    melt(id.vars=c('SAMPLE_TYPE'), variable.name='PARAMETER', value.name='RESULT') %>%
    mutate(PARAMETER=gsub('ASSESSMENT\\.', '', PARAMETER)) %>%
    select(SAMPLE_TYPE, PARAMETER, RESULT)
  
  return(aa)
  
}

organizeProfile <- function(parsedIn){
 # NEED TO FIND PARAMETERS THAT START WITH CHARACTER VS. NUMBER
  aa <- subset(parsedIn, select=str_starts(names(parsedIn),'PROFILE\\.[:alpha:]')) %>%
    mutate(LINE='0',SAMPLE_TYPE='HYDRO') %>%
    melt(id.vars=c('SAMPLE_TYPE','LINE'), variable.name='PARAMETER', value.name='RESULT') %>%
    select(SAMPLE_TYPE, LINE, PARAMETER, RESULT) %>%
    mutate(PARAMETER=str_replace(PARAMETER,'PROFILE\\.','')) %>%
    mutate(SAMPLE_TYPE=ifelse(str_starts(PARAMETER,'CLEAR_TO_BOTTOM|DISAPPEARS|REAPPEARS|SECCHI'),'SECC',SAMPLE_TYPE))
  # bb pulls out and formats species by line number and sample type
  bb <- subset(parsedIn, select=str_starts(names(parsedIn), 'PROFILE\\.[:digit:]')) %>%
    mutate(SAMPLE_TYPE='HYDRO') %>%
    melt(id.vars='SAMPLE_TYPE',value.name='RESULT') %>%
    mutate(variable = gsub('PROFILE\\.','',variable),
           LINE=str_extract(variable, '[:digit:]+')) %>%
    mutate(PARAMETER=str_replace(variable, '[:digit:]+\\_', '')) %>%
    select(SAMPLE_TYPE, LINE, PARAMETER, RESULT)
  # stack aa and bb on top of one another
  cc <- rbind(aa, bb) 
  
  return(cc)
  
}

organizeCalibration <- function(parsedIn){
  # Simply melt data and clean up parameter names
  aa <- mutate(parsedIn, SAMPLE_TYPE='CALIB') %>%
    melt(id.vars=c('SAMPLE_TYPE'), variable.name='PARAMETER', value.name='RESULT') %>%
    mutate(PARAMETER=gsub('CALIBRATION\\.', '', PARAMETER)) %>%
    select(SAMPLE_TYPE, PARAMETER, RESULT)
  
  return(aa)  
  
}

