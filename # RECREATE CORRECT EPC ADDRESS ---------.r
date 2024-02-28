library(dplyr)
library(readr)
library(stringr)

# RECREATE CORRECT EPC ADDRESS ------------------------------------------------- 
if (clean_addresses_epc == 1) {
  # Efficient file path creation and data reading
  file_count <- length(list.files("cleaned/epcs", full.names = TRUE))
  epc_file_path <- sprintf("cleaned/epc_addresses_%d.csv", file_count)
  epcs <- read_csv(epc_file_path, show_col_types = FALSE)
  
  # Optimized address splitting with separate and string operations
  epcs <- epcs %>%
    mutate(ADDRESS = str_trim(ADDRESS)) %>%
    separate(ADDRESS, into = c("PRIMARY", "STREET", "SECONDARY", "TERTIARY"), 
             sep = ",\\s*", remove = FALSE, extra = "merge") %>%
    separate(PRIMARY, into = c("PRIMARY_1", "PRIMARY_2", "PRIMARY_3", "PRIMARY_4"), 
             sep = "\\s+", remove = FALSE, extra = "merge") %>%
    rowwise() %>%
    mutate(
      PRIMARY = case_when(
        str_detect(PRIMARY_1, "^(Flat|FLAT|flat|Apartment)$") ~ STREET,
        TRUE ~ PRIMARY
      ),
      STREET = case_when(
        str_detect(PRIMARY_1, "^(Flat|FLAT|flat|Apartment)$") & !is.na(SECONDARY) ~ SECONDARY,
        TRUE ~ STREET
      ),
      SECONDARY = case_when(
        str_detect(PRIMARY_1, "^(Flat|FLAT|flat|Apartment)$") ~ paste(PRIMARY_1, PRIMARY_2),
        TRUE ~ SECONDARY
      )
    ) %>%
    ungroup() %>%
    select(-c(PRIMARY_1, PRIMARY_2, PRIMARY_3, PRIMARY_4)) %>%
    mutate(across(c(PRIMARY, STREET, SECONDARY, TERTIARY), toupper)) %>%
    mutate(STREET = str_replace_all(STREET, "\\.", "")) %>%
    arrange(POSTCODE, PRIMARY, SECONDARY)
  
  # Output the cleaned data
  output_file_path <- sprintf("cleaned/epc_addresses_%dc.csv", file_count - 1)
  write_csv(epcs, output_file_path)
}

# MATCH REGISTRY AND EPC ADDRESSES ---------------------------------------------
if (match_addresses == 1) {
  read_only <- 1
  merges_count <- 0
  
  # Loading data
  reg_file_path <- sprintf("cleaned/reg_addresses_%s.csv", registry_files)
  epc_file_path <- "cleaned/epc_addresses_345.csv"
  
  reg_addresses <- read_csv(reg_file_path, show_col_types = FALSE)
  epc_addresses <- read_csv(epc_file_path, show_col_types = FALSE)
  
  # Define a function to perform merge and update operations
  perform_merge <- function(reg, epc, merge_fields, merges_count) {
    merge_result <- merge(reg, epc, by = merge_fields)
    if (read_only == 0) {
      output_file_path <- sprintf("cleaned/addresses/merged_addresses_%d.csv", merges_count + 1)
      write_csv(merge_result, output_file_path)
    }
    return(list(merge_result = merge_result, merges_count = merges_count + 1))
  }
  
  # Example of using the perform_merge function
  merge_result <- perform_merge(reg_addresses, epc_addresses, c("PRIMARY", "SECONDARY", "STREET", "POSTCODE"), merges_count)
  exact <- merge_result$merge_result
  merges_count <- merge_result$merges_count
  
  # Further merge operations can follow the same pattern
  # Update epc_addresses and reg_addresses based on merge results as needed
  
  # Cleanup and final operations
  rm(exact) # Example of memory cleanup
  print(sprintf("%d merge operations completed", merges_count))
}