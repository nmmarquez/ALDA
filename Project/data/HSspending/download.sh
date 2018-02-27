echo "Bash version ${BASH_VERSION}..."
for i in {1998..2016..1}
    do
        ni=$((i + 1))
	ni_short=$(echo ${ni:2:4})
	i_short=$(echo ${i:2:4})
        if [ $i -lt 2003 ]
        then
            wget --user-agent=Safari -O - "https://www.cde.ca.gov/ds/fd/ec/documents/costofed$i_short$ni_short.xls" > spending_$i.xls
	elif [ $i -lt 2016 ]
        then
            wget --user-agent=Safari -O - "https://www.cde.ca.gov/ds/fd/ec/documents/currentexpense$i_short$ni_short.xls" > spending_$i.xls
        else
            wget --user-agent=Safari -O - "https://www.cde.ca.gov/ds/fd/ec/documents/currentexpense$i_short$ni_short.xlsx" > spending_$i.xlsx
    fi
done
