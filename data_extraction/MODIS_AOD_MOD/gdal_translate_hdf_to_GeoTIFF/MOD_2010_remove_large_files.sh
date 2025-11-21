#!/bin/bash

# Input path
SEARCH_PATH="/path/AOD_output/MOD/2010/"
LOG_FILE="MOD_2010_removed_files.log"

# Create or clear the log file
> "$LOG_FILE"

# Function to extract the identifier from a file name
extract_identifier() {
  basename "$1" | grep -oE 'MOD04_3K\.A[0-9]{7}\.[0-9]{4}\.[0-9]{3}\.[0-9]{13}'
}

export -f extract_identifier

# Find files over 200 MB and process them in parallel
find "$SEARCH_PATH" -type f -size +200M -print0 | parallel -0 -P 30 --no-notice '
  # Extract the identifier
  identifier=$(extract_identifier {})
  
  # Skip if no identifier is found
  if [ -z "$identifier" ]; then
    exit 0
  fi
  
  # Find all files with the same identifier
  find "$SEARCH_PATH" -type f -name "*$identifier*" -print | while read -r related_file; do
    # Remove the file if it exists
    if [ -e "$related_file" ]; then
      echo "Removing: $related_file" >> '"$LOG_FILE"'
      rm "$related_file"
    else
      echo "Already removed: $related_file" >> '"$LOG_FILE"'
    fi
  done
'

echo "Cleanup completed. Removed files are logged in $LOG_FILE."
