
#!/bin/bash
#--------------------------------------------
# author：Sijun Zhang
# last change date: 09/21/2019 
#--------------------------------------------

# INPUT SAMPLE: bash ./cutnames.sh ./recs2015_public_v4.csv 'DOEID\|^BRR' > recs_weights.csv

for arg in "$*"
do
    line=$arg
done

file=$(echo $line | tr " " "\n" | head --line=1)
expression=$(echo $line | tr " " "\n" | tail --line=1)

expression=$(echo $expression | sed 's/|/\\|/g' | tr -s '\\')

# DESCRIPTION: read the arguments from command line and select the first half as filename and the second half as the expression for columns selection

if [ ! -f "$file" ]; then
    wget https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v4.csv
fi

head --line=1 $file | tr "," "\n" > recs_names.txt

cols=$( grep -rn $expression recs_names.txt | tr ":" "\n" |awk 'NR%2 ==1' | tr '\n' ',' | awk '{sub(/.$/,"")}1' )

awk -F',' '{print $'$(echo $cols | sed 's/,/","$/g')' }' $file