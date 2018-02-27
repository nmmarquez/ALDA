

echo "Bash version ${BASH_VERSION}..."
for i in {1994..2015..1}
    do
	ni=$((i + 1))
	ni_short=$(echo ${ni:2:4})
	i_short=$(echo ${i:2:4})
	if [ $i -lt 2003 ]
	then
	    echo "http://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=$i_short$ni_short&cCat=UCGradsEth&cPage=filesgradaf.asp"
	    echo "http://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=$i_short$ni_short&cCat=GradsEth&cPage=filesgrad.asp"
	    wget --user-agent=Safari  -O - "http://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=$i_short$ni_short&cCat=UCGradsEth&cPage=filesgradaf.asp" > grad_elig_$i.txt
	    wget --user-agent=Safari  -O - "http://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=$i_short$ni_short&cCat=GradsEth&cPage=filesgrad.asp" > senior_$i.txt

	elif [ $i -lt 2006 ]
	then
	    echo "http://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=$i-$ni_short&cCat=UCGradsEth&cPage=filesgradaf.asp" 
        echo "http://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=$i-$ni_short&cCat=GradsEth&cPage=filesgrad.asp"
	    wget --user-agent=Safari  -O - "http://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=$i-$ni_short&cCat=UCGradsEth&cPage=filesgradaf.asp" > grad_elig_$i.txt
        wget --user-agent=Safari  -O - "http://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=$i-$ni_short&cCat=GradsEth&cPage=filesgrad.asp" > senior_$i.txt

    else
        echo "http://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=$i-$ni_short&cCat=UCGradEth&cPage=filesgradaf.asp"
        echo "http://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=$i-$ni_short&cCat=GradEth&cPage=filesgrad.asp"
        wget --user-agent=Safari  -O - "http://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=$i-$ni_short&cCat=UCGradEth&cPage=filesgradaf.asp" > grad_elig_$i.txt
        wget --user-agent=Safari  -O - "http://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=$i-$ni_short&cCat=GradEth&cPage=filesgrad.asp" > senior_$i.txt
    fi
done
