#!/bin/bash
#--------------------------------------------
# author：Sijun Zhang
# last change date: 09/21/2019 
#--------------------------------------------
# a Create a variable ‘file’ with the name of the csv file containing the RECS data. Check if this file exists in the local directory and, if not, download it.
file="recs2015_public_v4.csv"
if [ ! -f "$file" ]; then
    wget https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v4.csv
fi

# b Write a one-liner to extract the header row of the RECS data, translate the commas to new line characters, and write the results to a file ‘recs_names.txt’. [Hint: your solution should be a one-liner but you may wish to include in your script a code block that deletes recs_names.txt if the file already exists.]
head --line=1 $file | tr "," "\n" > recs_names.txt

# c Write a one-liner that uses ‘recs_names.txt’ to find the column positions for the id and replicate weight columns in the RECS data and then re-formats these positions as a single, comma-separated string. [Hint: For the final step, use the -s and -d options to the command paste.]
grep -rn 'DOEID\|BRRWT' recs_names.txt | tr ":" "\n" |awk 'NR%2 ==1' | tr '\n' ',' | awk '{sub(/.$/,"")}1'
# DESCRIPTION: firstly, I use grep to catch the index of id and replicate weight columns with the matched column names, then I transform the ':' to new line characters and place the index in the odd lines and the columns names in the even lines. Thus I can use the awk command to only output the index in the odd lines and transform the new line characters to commas. Finally I deleted the last rest comma in the string.

# d Store the result from the previous one-liner in the variable ‘cols’. Use this variable to write a one-liner that extracts the id and replicate weight columns from the recs data and writes them to recs_weights.csv. [Hint: In the first step, use a construction such as cols=$(...).]

cols=$( grep -rn 'DOEID\|BRRWT' recs_names.txt | tr ":" "\n" |awk 'NR%2 ==1' | tr '\n' ',' | awk '{sub(/.$/,"")}1' )
# DESCRIPTION: firstly, I store the result from the previous one-liner in the variable ‘cols’.
awk -F',' '{print $'$(echo $cols | sed 's/,/","$/g')' }' $file > recs_weights.csv
# DESCRIPTION: then I transform the $cols into suitable form like "1","$481.." to enable proper loading by awk. 