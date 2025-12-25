# Steps to generate the final dataset 

This document outlines the steps taken to generate the final dataset used in the statistical analyses for the voice-stress-pid5-ema project.

## Step 1: Imports raw mPath data files and consolidates them into a single dataset

Run `01_import_ema_mpath_data.R` to import the mPath raw data files and to consolidate them into a single dataset for further processing. The output is saved as `interim_data/ema_data_consolidated.RDS`.

## Step 2: Cleans and processes the consolidated dataset

Run `02_mpath_clean_ema_data.R` to perform the scoring of the PID-5 EMA dimensions of the consolidated dataset. This script takes as input the file `data/raw_complete/data_interim/ema_data_consolidated.RDS` and save the output as `data/raw_complete/data_interim/ema_data_scored.RDS`.

## Step 3: Merges cleaned EMA data with baseline personality data

Run `03_mpath_merge_ema_baseline_data.R` to merge the cleaned EMA data with baseline personality data. This script takes as input `data/raw_complete/data_interim/ema_data_scoring.RDS` and merges it with baseline personality measures to create a comprehensive dataset. The final dataset is saved as `data/raw_complete/data_final/final_dataset.RDS`.