library(data.table)
library(lubridate)

file_root <- "https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
years <- seq(1985, 2023)
tail <- ".txt.gz&dir=data/historical/stdmet/"

all_data <- list()

for (year in years) {
  path <- paste0(file_root, year, tail)
  
  # Read the first and second lines to determine how many lines to skip
  header <- scan(path, what = 'character', nlines = 1, quiet = TRUE)
  second_line <- scan(path, what = 'character', skip = 1, nlines = 1, quiet = TRUE)
  skip_lines <- ifelse(any(grepl("[a-zA-Z]", second_line)), 2, 1)
  
  # Read the data
  buoy <- fread(path, header = FALSE, skip = skip_lines, fill = TRUE)
  
  # Handle column names and ensure column numbers match
  if (length(header) == ncol(buoy)) {
    colnames(buoy) <- header
  } else if (length(header) < ncol(buoy)) {
    extra_cols <- paste0("V", (length(header) + 1):ncol(buoy))
    colnames(buoy) <- c(header, extra_cols)
  } else {
    colnames(buoy) <- header[1:ncol(buoy)]
  }
  
  year_col <- grep("^YY$|^YYYY$|^#YY$|^#YYYY$", colnames(buoy), ignore.case = TRUE)
  if (length(year_col) > 0) {
    setnames(buoy, year_col, "YYYY")
    # If the original column is a two-digit year, convert it to four digits
    if (max(buoy$YYYY, na.rm = TRUE) < 100) {
      buoy[, YYYY := ifelse(YYYY < 50, YYYY + 2000, YYYY + 1900)]
    }
  } else {
    # If no year column is found, add one
    buoy[, YYYY := year]
  }
  
  # Ensure month and day columns exist
  if (!"MM" %in% colnames(buoy)) buoy[, MM := NA]
  if (!"DD" %in% colnames(buoy)) buoy[, DD := NA]
  
  # Handle wind direction column (WD or WDIR)
  if ("WD" %in% colnames(buoy) && !"WDIR" %in% colnames(buoy)) {
    setnames(buoy, "WD", "WDIR")
  } else if ("WDIR" %in% colnames(buoy) && "WD" %in% colnames(buoy)) {
    buoy[, WDIR := ifelse(is.na(WDIR), WD, WDIR)]
    buoy[, WD := NULL]
  }
  
  # Handle pressure column (BAR or PRES)
  if ("BAR" %in% colnames(buoy) && !"PRES" %in% colnames(buoy)) {
    setnames(buoy, "BAR", "PRES")
  } else if ("PRES" %in% colnames(buoy) && "BAR" %in% colnames(buoy)) {
    buoy[, PRES := ifelse(is.na(PRES), BAR, PRES)]
    buoy[, BAR := NULL]
  } else if ("PRES" %in% colnames(buoy)) {
    # PRES column already exists, no need to modify
  } else if ("BAR" %in% colnames(buoy)) {
    setnames(buoy, "BAR", "PRES")
  }
  
  # Store data for each year
  all_data[[as.character(year)]] <- buoy
}


test_data <- rbindlist(all_data, use.names = TRUE, fill = TRUE)

# Ensure YYYY, MM, DD columns are at the front
setcolorder(test_data, c("YYYY", "MM", "DD"))


test_data[, DATE := ymd(paste(YYYY, MM, DD, sep = "-"))]


setcolorder(test_data, c("DATE", "YYYY", "MM", "DD"))


head(test_data)
