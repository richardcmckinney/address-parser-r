# RECREATE CORRECT EPC ADDRESS ------------------------------------------------- 
if (clean_addresses_epc == 1) {
  count <- length(list.files("cleaned/epcs",full.names=TRUE)) # should be equal to 345 (n files in cleaned folder)
  epcs <- read_csv(paste("cleaned/epc_addresses_",count,".csv",sep=""),show_col_types = FALSE)
  
  # Split address into PRIMARY, STREET and SECONDARY variables 
  epcs <- epcs %>% 
    separate(ADDRESS,sep = ", ",into = c("PRIMARY","STREET","SECONDARY","TERTIARY"), remove = FALSE)
  
  # check if the right property number was stored into the PRIMARY variable by splitting on SPACE delimiter
  epcs <- epcs %>%
    separate(PRIMARY,sep = " ",into = c("PRIMARY_1","PRIMARY_2","PRIMARY_3","PRIMARY_4"), remove = FALSE)
  
  # check if it successfully stored numbers in PRIMARY or if dealing with Flat/FLAT/flat numbers
  for (indice in 1:nrow(epcs)){ # loop on rows
    # ORDER OF CORRECTIONS IS ESSENTIAL!
    
    # CORRECTION 1:  we check if second split is equal to first
    if (epcs[indice,"PRIMARY"] != epcs[indice,"PRIMARY_1"]){
      # then check when different if it is a flat
      if (epcs[indice,"PRIMARY_1"] == "Flat" | epcs[indice,"PRIMARY_1"] == "FLAT" | epcs[indice,"PRIMARY_1"] == "flat"| epcs[indice,"PRIMARY_1"] == "Apartment"){ 
        # then check if building is stored in second split STREET
        if (epcs[indice,"PRIMARY"] == paste(epcs[indice,"PRIMARY_1"],epcs[indice,"PRIMARY_2"])) { 
          epcs[indice,"PRIMARY"] <- epcs[indice,"STREET"]
        } else { # if building is stored in first split as well
          epcs[indice,"PRIMARY"] <- str_sub(epcs[indice,"PRIMARY"],nchar(paste(epcs[indice,"PRIMARY_1"],epcs[indice,"PRIMARY_2"]))+2,-1)
        }
        # move SECONDARY to STREET but move STREET to TERTIARY first if PRIMARY_3 is not empty
        if (is.na(epcs[indice,"SECONDARY"]) == FALSE){
          if(is.na(epcs[indice,"SECONDARY"]) == FALSE){epcs[indice,"TERTIARY"] <- epcs[indice,"STREET"]} # keep extra information (probably not necessary)
          epcs[indice,"STREET"] <- epcs[indice,"SECONDARY"]
        } # need to check if SECONDARY is empty before replacing street
        
        # store keyword FLAT/APARTMENT in correct field
        if(epcs[indice,"PRIMARY_1"] == "Apartment"){
          epcs[indice,"SECONDARY"] <- paste("APARTMENT",epcs[indice,"PRIMARY_2"],sep="")
        } else {
          epcs[indice,"SECONDARY"] <- paste("FLAT",epcs[indice,"PRIMARY_2"],sep="")
        }
        # end if test on flat or apartment
      } else { # CORRECTION 2: WHEN NOT DEALING WITH A FLAT if we missed building and number
        # if none of PRIMARY_1 _2 and _3 are empty
        if (is.na(epcs[indice,"PRIMARY_1"]) == FALSE & is.na(epcs[indice,"PRIMARY_2"]) == FALSE & is.na(epcs[indice,"PRIMARY_3"]) == FALSE){
          epcs[indice,"SECONDARY"] <- epcs[indice,"PRIMARY_1"] # store number in secondary
          if(is.na(epcs[indice,"PRIMARY_4"]) == TRUE){ # store building in primary while PRIMARY_4 is empty
            epcs[indice,"PRIMARY"] <- paste(epcs[indice,"PRIMARY_2"],epcs[indice,"PRIMARY_3"]) 
          } else { epcs[indice,"PRIMARY"] <- paste(epcs[indice,"PRIMARY_2"],epcs[indice,"PRIMARY_3"],epcs[indice,"PRIMARY_4"]) } # store building while PRIMARY_4 is not empty
        } # end if above
      } # end else correction 2
    }  # end if correction 1
    
    
    # CORRECTION 3:  we check if separator was " " instead of ", ", i.e. STREET is initially empty
    if (is.na(epcs[indice,"STREET"])){
      # store correct street in STREET
      if(is.na(epcs[indice,"PRIMARY_4"]) == TRUE){ #  while PRIMARY_4 is empty
        epcs[indice,"STREET"] <- paste(epcs[indice,"PRIMARY_2"],epcs[indice,"PRIMARY_3"])
        epcs[indice,"SECONDARY"] <- NA
        epcs[indice,"PRIMARY"] <- epcs[indice,"PRIMARY_1"] # store number in PRIMARY
      } else { #  while PRIMARY_4 is not empty
        epcs[indice,"STREET"] <- paste(epcs[indice,"PRIMARY_3"],epcs[indice,"PRIMARY_4"])
        epcs[indice,"SECONDARY"] <- epcs[indice,"PRIMARY_1"] # store flat number in SECONDARY
        epcs[indice,"PRIMARY"] <- epcs[indice,"PRIMARY_2"] # store number in PRIMARY
      }
    }  # end if correction 3
    
    # CORRECTION 4: if STREET and PRIMARY are equal then STREET contains a number (which should be in PRIMARY)
    if (epcs[indice,"PRIMARY"] == epcs[indice,"STREET"]){
      epcs[indice,"PRIMARY"] <- gsub( " .*$", "", epcs[indice,"PRIMARY"] ) # store number in PRIMARY, which is before 1st space
      epcs[indice,"STREET"] <- str_sub(epcs[indice,"STREET"],nchar(epcs[indice,"PRIMARY_1"]),-1) # select all after 1st space in STREET
    } # end correction 4
    
  } # end of loop on nrows
  
  # CORRECTION 5: Change Variables to Upper case (also need to add/remove "." in "St.")
  epcs <- epcs %>%
    mutate(
      STREET = toupper(STREET),
      PRIMARY = toupper(PRIMARY),
      SECONDARY = toupper(SECONDARY),
      TERTIARY = toupper(TERTIARY)
    )
  
  # remove temporary variables
  epcs <- subset(epcs, select = -c(PRIMARY_1,PRIMARY_2,PRIMARY_3,PRIMARY_4))
  
  # Clean addresses from special characters
  epcs$STREET <- gsub("\\.", "", epcs$STREET)
  
  # Order data
  epcs <- epcs[order(epcs$POSTCODE,epcs$PRIMARY,epcs$SECONDARY),]
  
  # output file with cleaned addresses
  write_csv(epcs, paste("cleaned/epc_addresses_",count-1,"c",".csv",sep="")) # remove one extra count
  
} # end if on clean_addresses


# MATCH REGISTRY AND EPC ADDRESSES ---------------------------------------------
if (match_addresses == 1) {   
  
  read_only <- 1    # 1 if read only, 0 if write over existing files
  merges_count <- 0 # INIT to 0
  
  reg_addresses <- fread(paste("cleaned/reg_addresses_",registry_files,".csv",sep="")) # load registry addresses
  epc_addresses <- fread("cleaned/epc_addresses_345.csv") # load epc addresses
  
  # Clean addresses from special characters
  #epc_addresses$STREET <- gsub(" NA", "", epc_addresses$STREET)
  
  # MERGE on 4 columns -------------------------------------------------------
  merges_count <- merges_count + 1
  if (read_only == 0){
    exact <- merge(x = reg_addresses, y = epc_addresses, by = c("PRIMARY","SECONDARY","STREET","POSTCODE"))
    print(paste(nrow(exact),"addresses matched, merge count =",merges_count)) # display number of addresses merged
    write_csv(exact, paste("cleaned/addresses/merged_addresses_",merges_count,".csv",sep = "")) # output file
  } else {exact <- fread(paste("cleaned/addresses/merged_addresses_",merges_count,".csv",sep = ""))}
  
  epc_addresses <- epc_addresses[!epc_addresses$BUILDING_REFERENCE_NUMBER%in%exact$BUILDING_REFERENCE_NUMBER.y,] # remove inner merge from epc_addresses (since exact match)
  rm(exact) # free memory
  
  # MERGE on 4 columns -------------------------------------------------------
  merges_count <- merges_count + 1
  if (read_only == 0){
    # for FLAT/APARTMENTS only: rearrange TERTIARY into PRIMARY, e.g. BRIMSTONE HOUSE, 10 after removing duplicates in TERTIARY
    epc_addresses$PRIMARY <- ifelse(!is.na(epc_addresses$TERTIARY) & !is.na(as.numeric(epc_addresses$TERTIARY)),paste(epc_addresses$PRIMARY,epc_addresses$TERTIARY,sep =", "),epc_addresses$PRIMARY)
    
    exact <- merge(x = reg_addresses, y = epc_addresses, by = c("PRIMARY","SECONDARY","STREET","POSTCODE"))
    print(paste(nrow(exact),"addresses matched, merge count =",merges_count)) # display number of addresses merged
    write_csv(exact, paste("cleaned/addresses/merged_addresses_",merges_count,".csv",sep = "")) # output file
  } else {exact <- fread(paste("cleaned/addresses/merged_addresses_",merges_count,".csv",sep = ""))}
  
  epc_addresses <- epc_addresses[!epc_addresses$BUILDING_REFERENCE_NUMBER%in%exact$BUILDING_REFERENCE_NUMBER.y,] # remove inner merge from epc_addresses (since exact match)
  rm(exact) # free memory
  
  # MERGE on primary secondary and postcode ----------------------------------
  merges_count <- merges_count + 1
  if (read_only == 0){
    exact <- merge(x = reg_addresses, y = epc_addresses, by = c("PRIMARY","SECONDARY","POSTCODE")) 
    print(paste(nrow(exact),"addresses matched, merge count =",merges_count)) # display number of addresses merged
    write_csv(exact, paste("cleaned/addresses/merged_addresses_",merges_count,".csv",sep = "")) # output file
  } else {exact <- fread(paste("cleaned/addresses/merged_addresses_",merges_count,".csv",sep = ""))}
  
  epc_addresses <- epc_addresses[!epc_addresses$BUILDING_REFERENCE_NUMBER%in%exact$BUILDING_REFERENCE_NUMBER.y,] # remove inner merge from epc_addresses (since exact match)
  rm(exact) # free memory
  
  ## MERGES on 3 columns HOUSES ----------------------------------------------
  # Only house remaining: successive permutations between PRIMARY/SECONDARY/STREET then merge on PRIMARY/STREET/POSTCODE
  
  # REMOVE ALL flats/apartments to avoid matching without the secondary number
  epc_addresses <- epc_addresses[!grepl("FLAT",epc_addresses$SECONDARY),]
  epc_addresses <- epc_addresses[!grepl("APARTMENT",epc_addresses$SECONDARY),]
  reg_addresses <- reg_addresses[!grepl("FLAT",reg_addresses$SECONDARY),]
  reg_addresses <- reg_addresses[!grepl("APARTMENT",reg_addresses$SECONDARY),]
  
  # PERMUTE secondary with primary -------------------------------------------
  epc_addresses2 <- epc_addresses %>%
    mutate(
      temp = ifelse(is.na(SECONDARY),NA,SECONDARY), # temp variable
      SECONDARY = ifelse(is.na(PRIMARY),NA,PRIMARY), # permutation
      PRIMARY = ifelse(is.na(temp),NA,temp) # move temp 
    )
  epc_addresses2 <- subset(epc_addresses2, select = -c(temp) ) # remove filter columns
  
  # MERGE on primary secondary and postcode ----------------------------------
  merges_count <- merges_count + 1
  exact <- merge(x = reg_addresses, y = epc_addresses2, by = c("PRIMARY","SECONDARY","POSTCODE"))
  print(paste(nrow(exact),"addresses matched, merge count =",merges_count)) # display number of addresses merged
  write_csv(exact, paste("cleaned/addresses/merged_addresses_",merges_count,".csv",sep = "")) # output file
  
  epc_addresses <- epc_addresses[!epc_addresses$BUILDING_REFERENCE_NUMBER%in%exact$BUILDING_REFERENCE_NUMBER.y,] # remove inner merge from epc_addresses (since exact match)
  rm(exact) # free memory
  
  # PERMUTE secondary with street --------------------------------------------
  epc_addresses2 <- epc_addresses %>%
    mutate(
      temp = ifelse(is.na(SECONDARY),NA,SECONDARY), # temp variable
      SECONDARY = ifelse(is.na(STREET),NA,STREET), # permutation
      STREET = ifelse(is.na(temp),NA,temp) # move temp 
    )
  epc_addresses2 <- subset(epc_addresses2, select = -c(temp) ) # remove filter columns
  
  # MERGE on primary secondary and postcode ----------------------------------
  merges_count <- merges_count + 1
  exact <- merge(x = reg_addresses, y = epc_addresses2, by = c("PRIMARY","SECONDARY","POSTCODE"))
  ### then remove duplicates on BUILDING_REFERENCE_NUMBER since inexact merge
  n_occur <- data.frame(table(exact$BUILDING_REFERENCE_NUMBER))
  exact <- exact[exact$BUILDING_REFERENCE_NUMBER %in% n_occur$Var1[n_occur$Freq == 1],]
  print(paste(nrow(exact),"addresses matched, merge count =",merges_count)) # display number of addresses merged
  write_csv(exact, paste("cleaned/addresses/merged_addresses_",merges_count,".csv",sep = "")) # output file
  
  epc_addresses <- epc_addresses[!epc_addresses$BUILDING_REFERENCE_NUMBER%in%exact$BUILDING_REFERENCE_NUMBER.y,] # remove inner merge from epc_addresses (since exact match)
  rm(exact) # free memory
  
  # TWO REPLACE --------------------------------------------------------------
  # permute primary with street AND secondary with primary
  epc_addresses2 <- epc_addresses %>% # need to input epc_addresses
    mutate(
      temp = ifelse(is.na(SECONDARY),NA,SECONDARY), # temp variable
      SECONDARY = ifelse(is.na(PRIMARY),NA,PRIMARY), # permutation
      PRIMARY = ifelse(is.na(STREET),NA,STREET), # permutation
      STREET = ifelse(is.na(temp),NA,temp) # move temp 
    )
  epc_addresses2 <- subset(epc_addresses2, select = -c(temp) ) # remove filter columns
  
  # MERGE on primary secondary and postcode ----------------------------------
  merges_count <- merges_count + 1
  exact <- merge(x = reg_addresses, y = epc_addresses2, by = c("PRIMARY","SECONDARY","POSTCODE"))
  print(paste(nrow(exact),"addresses matched, merge count =",merges_count)) # display number of addresses merged
  write_csv(exact, paste("cleaned/addresses/merged_addresses_",merges_count,".csv",sep = "")) # output file
  
  epc_addresses <- epc_addresses[!epc_addresses$BUILDING_REFERENCE_NUMBER%in%exact$BUILDING_REFERENCE_NUMBER.y,] # remove inner merge from epc_addresses (since exact match)
  rm(exact) # free memory
  
  # TWO REPLACE --------------------------------------------------------------
  # permute primary with street AND secondary with primary
  epc_addresses2 <- epc_addresses %>% # need to input epc_addresses
    mutate(
      temp = ifelse(is.na(PRIMARY),NA,PRIMARY), # temp variable
      PRIMARY = ifelse(is.na(SECONDARY),NA,SECONDARY), # permutation
      SECONDARY = ifelse(is.na(STREET),NA,STREET), # permutation
      STREET = ifelse(is.na(temp),NA,temp) # move temp 
    )
  epc_addresses2 <- subset(epc_addresses2, select = -c(temp) ) # remove filter columns
  
  # MERGE on primary secondary and postcode ----------------------------------
  merges_count <- merges_count + 1
  exact <- merge(x = reg_addresses, y = epc_addresses2, by = c("PRIMARY","SECONDARY","POSTCODE"))
  ### remove duplicates on BUILDING_REFERENCE_NUMBER since inexact merge
  n_occur <- data.frame(table(exact$BUILDING_REFERENCE_NUMBER))
  exact <- exact[exact$BUILDING_REFERENCE_NUMBER %in% n_occur$Var1[n_occur$Freq == 1],]
  print(paste(nrow(exact),"addresses matched, merge count =",merges_count)) # display number of addresses merged
  write_csv(exact, paste("cleaned/addresses/merged_addresses_",merges_count,".csv",sep = "")) # output file
  
  epc_addresses <- epc_addresses[!epc_addresses$BUILDING_REFERENCE_NUMBER%in%exact$BUILDING_REFERENCE_NUMBER.y,] # remove inner merge from epc_addresses (since exact match)
  rm(exact) # free memory
  
  ### remove registry with something in secondary to avoid over-merging with street while without secondary
  reg_addresses <- reg_addresses[is.na(reg_addresses$SECONDARY),] 
  
  # MERGE on PRIMARY, STREET and POSTCODE ------------------------------------
  merges_count <- merges_count + 1
  exact <- merge(x = reg_addresses, y = epc_addresses, by = c("PRIMARY","STREET","POSTCODE"))
  print(paste(nrow(exact),"addresses matched, merge count =",merges_count)) # display number of addresses merged
  write_csv(exact, paste("cleaned/addresses/merged_addresses_",merges_count,".csv",sep = "")) # output file
  
  epc_addresses <- epc_addresses[!epc_addresses$BUILDING_REFERENCE_NUMBER%in%exact$BUILDING_REFERENCE_NUMBER.y,] # remove inner merge from epc_addresses (since exact match)
  rm(exact) # free memory
  
  # TWO REPLACE -------------------------------------------------------------
  # permute primary with street AND secondary with primary
  epc_addresses2 <- epc_addresses %>% # need to input epc_addresses
    mutate(
      temp = ifelse(is.na(PRIMARY),NA,PRIMARY), # temp variable
      PRIMARY = ifelse(is.na(SECONDARY),NA,SECONDARY), # permutation
      SECONDARY = ifelse(is.na(STREET),NA,STREET), # permutation
      STREET = ifelse(is.na(temp),NA,temp) # move temp 
    )
  epc_addresses2 <- subset(epc_addresses2, select = -c(temp) ) # remove filter columns
  
  # MERGE on PRIMARY, STREET and POSTCODE ------------------------------------
  merges_count <- merges_count + 1
  exact <- merge(x = reg_addresses, y = epc_addresses2, by = c("PRIMARY","STREET","POSTCODE"))
  print(paste(nrow(exact),"addresses matched, merge count =",merges_count)) # display number of addresses merged
  write_csv(exact, paste("cleaned/addresses/merged_addresses_",merges_count,".csv",sep = "")) # output file
  
  epc_addresses <- epc_addresses[!epc_addresses$BUILDING_REFERENCE_NUMBER%in%exact$BUILDING_REFERENCE_NUMBER.y,] # remove inner merge from epc_addresses (since exact match)
  rm(exact) # free memory
  
  rm(epc_addresses,epc_addresses2,reg_addresses) # clean section
  print(paste(merges_count,"merge operations completed"))
  
} # end if on match_addresses