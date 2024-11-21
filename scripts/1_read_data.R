# At the beginning of each script
source("scripts/0_setup.R")


# Function to read data from CSV without standardizing column names
read_data <- function(file_path) {
  data <- read_csv(file_path)
  return(data)
}

# Function to read all datasets from a folder and return them as a list
load_datasets <- function(directory = "data/raw/historical") {
  # List all CSV files in the directory
  file_list <- list.files(path = directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Initialize an empty list to store data frames
  data_list <- list()
  
  # Loop through each file, read the data, and store it in the list
  for (file_path in file_list) {
    # Generate a unique name based on the file name (remove directory path and ".csv" extension)
    data_name <- tools::file_path_sans_ext(basename(file_path))
    
    # Read the data and add it to the list
    data_list[[data_name]] <- read_data(file_path)
  }
  
  # Return the list of data frames
  return(data_list)
}

# Load datasets by calling the function and storing the result
datasets <- load_datasets()

names(datasets)

data_19_raw <- datasets$Umfrage19jittered
data_20_raw <- datasets$Umfrage20jittered
data_21_raw <- datasets$Umfrage21jittered
data_22_raw <- datasets$Umfrage22jittered
data_23_raw <- datasets$Umfrage23jittered
data_24_raw <- datasets$Umfrage24jittered


