#!/bin/bash
#--------------------------------------------
# author：Sijun Zhang
# last change date: 09/21/2019 
#--------------------------------------------
# a Create a variable ‘file’ with the name of the csv file containing the RECS data. Check if this file exists in the local directory and, if not, download it.
file="recs2015_public_v4.csv"
if [! -f "$file"]; then
    # wget https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v4.csv
    echo '$file'
fi