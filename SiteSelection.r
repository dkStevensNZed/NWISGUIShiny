getAllStations <- function(selSubset=NULL){
staID <- staName <- staDesc <- vector()

  staID[1] <- '11185185'  #MLD
  staID[2] <- '11304810'  #STK
  staID[3] <- '11311300'  #TRN
  staID[4] <- '11312672'  #VIC
  staID[5] <- '11312676'  #MID
  staID[6] <- '11312685'  #MRC
  staID[7] <- '11312968'  #DMC
  staID[8] <- '11313200'  #GLW
  staID[9] <- '11313240'  #GLW2
  staID[10] <- '11313315'  #ORF
  staID[11] <- '11313405'  #OLD
  staID[12] <- '11313431'  #HOL
  staID[13] <- '11313433'  #DCH
  staID[14] <- '11313434'  #ORQ
  staID[15] <- '11313440'  #FAL
  staID[16] <- '11313452'  #ORS
  staID[17] <- '11313460'  #PPT
  staID[18] <- '11336600'  #DCC
  staID[19] <- '11336680'  #SFM
  staID[20] <- '11336685'  #NFM
  staID[21] <- '11336790'  #LPS
  staID[22] <- '11336930'  #MOK
  staID[23] <- '11337080'  #TMN
  staID[24] <- '11337190'  #JPT
  staID[25] <- '11447650'  #FPT
  staID[26] <- '11447830'  #SUT
  staID[27] <- '11447850'  #STM
  staID[28] <- '11447890'  #WGA
  staID[29] <- '11447903'  #GEO
  staID[30] <- '11447905'  #WGB
  staID[31] <- '11455165'  #MIN
  staID[32] <- '14455315'  #LIB
  staID[33] <- '11455335'  #DWS
  staID[34] <- '11455350'  #CCH
  staID[35] <- '11455420'  #RIO
  staID[36] <- '11455478'  #DEC
  staID[37] <- '11162765'  #SM San Mateo
  staID[38] <- '375607122264701'  #RICH Richmond-San Rafael
  staID[39] <- '373015122071000'  #DUM Dumbarton
  staID[40] <- '374811122235001'  #P17 pier 17
  staID[41] <- '374938122251801'  #ALC Alcatraz
  staID[42] <- '11455820'  #CAR carquinez
  staID[43] <- '11455780'  #BEN benicia
  staID[44] <- '11169750'  #ALV alviso slough
  
  
  
  staName[1] <- 'MLD'
  staName[2] <- 'STK'
  staName[3] <- 'TRN'
  staName[4] <- 'VIC'
  staName[5] <- 'MID'
  staName[6] <- 'MRC'
  staName[7] <- 'DMC'
  staName[8] <- 'GLW'
  staName[9] <- 'GLW2'
  staName[10] <- 'ORF'
  staName[11] <- 'OLD'
  staName[12] <- 'HOL'
  staName[13] <- 'DCH'
  staName[14] <- 'ORQ'
  staName[15] <- 'FAL'
  staName[16] <- 'ORS'
  staName[17] <- 'PPT'
  staName[18] <- 'DCC'
  staName[19] <- 'SFM'
  staName[20] <- 'NFM'
  staName[21] <- 'LPS'
  staName[22] <- 'MOK'
  staName[23] <- 'TMN'
  staName[24] <- 'JPT'
  staName[25] <- 'FPT'
  staName[26] <- 'SUT'
  staName[27] <- 'STM'
  staName[28] <- 'WGA'
  staName[29] <- 'GEO'
  staName[30] <- 'WGB'
  staName[31] <- 'MIN'
  staName[32] <- 'LIB'
  staName[33] <- 'DWS'
  staName[34] <- 'CCH'
  staName[35] <- 'RIO'
  staName[36] <- 'DEC'
  staName[37] <- 'SM'
  staName[38] <- 'RICH'
  staName[39] <- 'DUM'
  staName[40] <- 'P17'
  staName[41] <- 'ALC'
  staName[42] <- 'CAR'
  staName[43] <- 'BEN'
  staName[44] <- 'ALV'
  
  staDesc = rep(NA,times=length(staID))
  staDesc[37:44] <- c('San Mateo Br.','Richmond/San Raphael','Dumbarton Br.',
                      'Pier 17','Alcatraz','Carquinez','Benicia','Alviso slough')
  stn.all.df <- data.frame(Site_no = staID, Site_name = staName, Site_desc = staDesc, 
                           Lat = NA, Long = NA, Depth.1 = NA, Depth.2 = NA)
  
  stn.df <- data.frame()
  if(exists('selSubset')) 
    stn.df <- stn.all.df[which(stn.all.df$Site_name %in% selSubset),]
  return(list(All=stn.all.df,Selected=stn.df))

}