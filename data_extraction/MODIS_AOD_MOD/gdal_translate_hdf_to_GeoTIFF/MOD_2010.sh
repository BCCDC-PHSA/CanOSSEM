#!/bin/bash

# Input and output base directories
base_input_dir="/path/unprocessed_raw_input_data/AOD_input/MOD/2010"
base_output_dir="/path/AOD_output/MOD/2010"
log_file="MOD_processed_files.log"

# Ensure the log file exists
touch "$log_file"

# Find all HDF files
find "$base_input_dir" -type f -name "*.hdf" > hdf_files_2010.txt

# Function to process a single HDF file
process_hdf() {
    local hdf_file=$1

    # Check if the file is already processed
    if grep -Fxq "$hdf_file" "$log_file"; then
        echo "Skipping already processed file: $hdf_file"
        return
    fi

    # Extract relative path and base filename
    local relative_path=$(dirname "${hdf_file#$base_input_dir/}")
    local base_name=$(basename "$hdf_file" .hdf)

    # Create corresponding output directory
    local output_dir="${base_output_dir}/${relative_path}"
    mkdir -p "$output_dir"

    # Declare subdatasets
    declare -A subdatasets=(
        ["Scan_Start_Time"]="mod04:Scan_Start_Time"
        ["Corrected_Optical_Depth_Land"]="mod04:Corrected_Optical_Depth_Land"
        ["Corrected_Optical_Depth_Land_wav2p1"]="mod04:Corrected_Optical_Depth_Land_wav2p1"
        ["Mass_Concentration_Land"]="mod04:Mass_Concentration_Land"
    )

    # Process subdatasets
    for subdataset_name in "${!subdatasets[@]}"; do
        subdataset_path=${subdatasets[$subdataset_name]}
        subdataset_full="HDF4_EOS:EOS_SWATH:\"$hdf_file\":$subdataset_path"

        # Check subdataset dimensions
        dimensions=$(gdalinfo "$subdataset_full" | grep "Size is" | awk '{print $3"x"$4}' | tr -d ",")
        if [[ -z "$dimensions" || "$dimensions" == "0x1" ]]; then
            echo "Skipping $subdataset_name from $hdf_file: Invalid or empty subdataset"
            continue
        fi

        # Multi-band handling
        if [[ "$subdataset_name" == "Corrected_Optical_Depth_Land" ]]; then
            for band in 1 2 3; do
                temp_output_file="${output_dir}/${base_name}_${subdataset_name}_Band${band}.tif"
                rectified_output_file="${output_dir}/${base_name}_${subdataset_name}_Band${band}_rectified.tif"

                gdal_translate -b $band -of GTiff "$subdataset_full" "$temp_output_file" &&
                    gdalwarp -t_srs EPSG:4326 -r bilinear "$temp_output_file" "$rectified_output_file" &&
                    rm "$temp_output_file" &&
                    echo "Processed Band $band: $rectified_output_file"
            done
        else
            temp_output_file="${output_dir}/${base_name}_${subdataset_name}.tif"
            rectified_output_file="${output_dir}/${base_name}_${subdataset_name}_rectified.tif"

            gdal_translate -of GTiff "$subdataset_full" "$temp_output_file" &&
                gdalwarp -t_srs EPSG:4326 -r bilinear "$temp_output_file" "$rectified_output_file" &&
                rm "$temp_output_file" &&
                echo "Processed $subdataset_name: $rectified_output_file"
        fi
    done

    # Log the successfully processed file
    echo "$hdf_file" >> "$log_file"
}

export -f process_hdf
export base_input_dir
export base_output_dir
export log_file

# Run in parallel using GNU Parallel
parallel -j 32  process_hdf :::: hdf_files_2010.txt
