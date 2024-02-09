## setting directory to this source file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(uuid)
library(tidyverse)
library(readxl)
library(DBI)
library(RSQLite)
library(stringr)

## Standard to Hex Conversion
Hex <- function(vgs5_guid) {
  ## converting to hex
  guid<- vgs5_guid
  # Remove curly braces and hyphens
  guid <- gsub("[{}-]", "", guid)
  
  # Convert to hexadecimal
  hex_guid <- tolower(paste0(substr(guid, 7, 8), substr(guid, 5, 6), substr(guid, 3, 4),
                             substr(guid, 1, 2),substr(guid,11,12),substr(guid,9,10),
                             substr(guid,15,16),substr(guid,13,14),
                             gsub("-", "", substr(guid, 17, 36))))
  
  hex_guid <- paste0("X'",hex_guid,"'")
  # Return the result
  return(hex_guid)
}


## Ordering Lists by UI selection
sort_by <- input$order_by

if (length(sort_by) > 0) {
  ## SQL local Connection info from R to local VGS5 (VGS50.db)
  #db_loc <- "C:/ProgramData/VGSData/VGS50.db"
  #mydb <- dbConnect(RSQLite::SQLite(), dbname = db_loc)
  
  ## looking at all list on local VGS
  list_names_soils <- paste0(
    "Select DISTINCT ListName, quote(PK_spList) from spList
inner join splistLink on spListLink.FK_SpList = spList.PK_spList
where spList.SpFilter = 'OT'"
  )
  
  soil_lists<- dbGetQuery(mydb,list_names_soils)
  
  
  ## getting each species by list
  i=1
  while (i < nrow(soil_lists)+1) {

    species_by_list <- paste0("Select quote(PK_SpListLink), Species.* from spList
inner join splistLink on spListLink.FK_SpList = spList.PK_spList
inner join species on species.PK_Species = spListLink.FK_Species
where PK_spList = ",soil_lists$`quote(PK_spList)`[i]," order by ",sort_by)
    ## get species being used by each list
    species<- dbGetQuery(mydb,species_by_list)
    
    print(paste0("Working on list ",soil_lists$ListName[i]," - ",i,"/",nrow(soil_lists)))
    
    x=1
    while (x < nrow(species)+1) {
      ## update order of species by adding weights to spListLink
      update_list <- paste0("Update spListLink
                        Set weight = ",x," 
                        Where PK_SpListLink = ",species$`quote(PK_SpListLink)`[x])
      ##execute
      dbExecute(mydb, update_list)
      x=x+1
    }
    
    print("List species weight updated to ABC order")
    i=i+1
    print("Process Complete")
    
  }
  
  ## closing SQL connection
  #DBI::dbDisconnect(mydb)
  #closeAllConnections()
  
}





