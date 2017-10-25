#wget http://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=2015-16&cCat=UCGradEth&cPage=filesgradaf.asp

echo "Bash version ${BASH_VERSION}..."
for i in {1994..2015..1}
    do
	ni=$((i + 1))
	ni_short=$(echo ${ni:2:4})
	i_short=$(echo ${i:2:4})
	if [ $i -lt 2003 ]
	then
	    curl "http://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=$i_short$ni_short&cCat=UCGradsEth&cPage=filesgradaf.asp" > grad_elig_$i.txt
	    curl "http://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=$i_short$ni_short&cCat=GradsEth&cPage=filesgrad.asp" > senior_$i.txt

	elif [ $i -lt 2006 ]
	then
	    curl "http://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=$i-$ni_short&cCat=UCGradsEth&cPage=filesgradaf.asp" > grad_elig_$i.txt
            curl "http://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=$i-$ni_short&cCat=GradsEth&cPage=filesgrad.asp" > senior_$i.txt

        else
	    curl "http://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=$i-$ni_short&cCat=UCGradEth&cPage=filesgradaf.asp" > grad_elig_$i.txt
            curl "http://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=$i-$ni_short&cCat=GradEth&cPage=filesgrad.asp" > senior_$i.txt
        fi
done
