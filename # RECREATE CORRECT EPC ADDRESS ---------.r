library(dplyr)
library(readr)
library(stringr)

# ------------------ Helper Functions ---------------------

# Cleans a single address string by trimming, removing special characters, and converting to uppercase
clean_address <- function(address) {
  address %>%
    str_trim() %>% # Remove leading and trailing whitespace
    str_remove_all("\\.") %>% # Remove periods
    str_to_upper() # Convert to uppercase
}

# Corrects address fields within a dataframe
correct_address <- function(df) {
  df %>%
    mutate(ADDRESS = clean_address(ADDRESS)) %>%
    # Split address into components. The 'extra' argument is set to 'merge' to combine all extra pieces into the last component
    separate(ADDRESS, sep = ",\\s*", into = c("PRIMARY", "STREET", "SECONDARY", "TERTIARY"), 
             remove = FALSE, extra = "merge") %>%
    # Further separate the PRIMARY field to identify different parts of the address. This is particularly useful for complex addresses
    separate(PRIMARY, into = c("PRIMARY_1", "PRIMARY_2", "PRIMARY_3", "PRIMARY_4"), 
             sep = "\\s+", remove = FALSE, extra = "merge") %>%
    # Apply row-wise operations to restructure the address based on certain conditions
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
    # Remove the temporary columns used for address restructuring
    select(-c(PRIMARY_1, PRIMARY_2, PRIMARY_3, PRIMARY_4)) %>%
    # Apply the clean_address function to all address-related fields
    mutate(across(c(PRIMARY, STREET, SECONDARY, TERTIARY), clean_address)) %>%
    # Sort the data by postcode and then by primary and secondary address components for consistency
    arrange(POSTCODE, PRIMARY, SECONDARY)
}

# Merges two datasets and writes the output to a file if an output prefix is provided
perform_merge <- function(reg_data, epc_data, merge_fields, output_prefix = NULL) {
  merge_result <- full_join(reg_data, epc_data, by = merge_fields)
  # Write the merged result to a file if an output path is provided
  if (!is.null(output_prefix)) {
    output_file_path <- sprintf("%s_%d.csv", output_prefix, nrow(merge_result))
    write_csv(merge_result, output_file_path)
  }
  merge_result
}

# ------------------ Main Execution ---------------------

# Check if 'clean_addresses_epc' flag is set to proceed with cleaning EPC addresses
if (clean_addresses_epc == 1) {
  # Construct file path and read in the EPC data
  epc_file_path <- sprintf("cleaned/epc_addresses_%d.csv", length(list.files("cleaned/epcs", full.names = TRUE)))
  epcs <- read_csv(epc_file_path, show_col_types = FALSE)
  # Clean and correct the address data within the dataframe
  epcs_corrected <- correct_address(epcs)
  # Write the cleaned data to a new file
  write_csv(epcs_corrected, sprintf("cleaned/epc_addresses_%dc.csv", length(list.files("cleaned/epcs", full.names = TRUE)) - 1))
}

# Check if 'match_addresses' flag is set to proceed with merging address datasets
if (match_addresses == 1) {
  # Read in registry and EPC data, cleaning addresses in the process
  reg_file_path <- sprintf("cleaned/reg_addresses_%s.csv", registry_files)
  reg_addresses <- read_csv(reg_file_path, show_col_types = FALSE) %>% correct_address()
  epc_addresses <- read_csv("cleaned/epc_addresses_345.csv", show_col_types = FALSE) %>% correct_address()

  # Check if we are in read-only mode, which would skip writing the merge result to a file.
  # The 'read_only' flag should be defined elsewhere in the script.
  merges_count <- 0
  result1 <- perform_merge(registry_addresses, epc_addresses, c("PRIMARY", "SECONDARY", "STREET", "POSTCODE"), 
                           if (!read_only) file.path(output_directory, "merged_addresses") else NULL)

  # ... (potential additional merge operations)

  if (!read_only) {
    print("Merge operations completed") 
  }
}