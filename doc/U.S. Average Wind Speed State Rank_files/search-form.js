function loadItemsToRank(itemToHighlight,exType){
	itrHTML = "<div><span class=\"big\"><b>Pick a Topic to Rank:</b></span>";
	rankingDesc = "";
	rankOf = "";
	if (itemToHighlight.length>0){
		document.rsF.itemtorank.value=itemToHighlight;
	}else{
		itemToHighlight=document.rsF.itemtorank.value;
	}

	if (itemToHighlight.length>0){
		for (var i = 0; i < ra.length; i++) {
			if (ra[i][1]==itemToHighlight){
				//itrHTML += ' '+ra[i][2];
				rankingDesc = ra[i][2];
				rankOf = ra[i][3];
			}
		}
	}
	itrHTML += "</div>";
	for (var j = 0; j < 5; j++) {
		//if (j==0) itrHTML += "<div><b>Popular Topics:</b></div>";
		if (j==0){
			itrHTML += "<div style=\"padding-left:10px;\"><a href=\"#\" onClick=\"loadItemsToRank('',";
			if (j==exType){itrHTML += "-1);return false;\"><img src=\"/img/minus.png\">";}else{itrHTML += "0);return false;\"><img src=\"/img/plus.png\">";}
			itrHTML += " <b>Popular Topics</b></a></div>";
		}
		if (j==1){
			itrHTML += "<div style=\"padding-left:10px;\"><a href=\"#\" onClick=\"loadItemsToRank('',";
			if (j==exType){itrHTML += "-1);return false;\"><img src=\"/img/minus.png\">";}else{itrHTML += "1);return false;\"><img src=\"/img/plus.png\">";}
			itrHTML += " <b>Population and Races</b></a></div>";
		}
		if (j==2){
			itrHTML += "<div style=\"padding-left:10px;\"><a href=\"#\" onClick=\"loadItemsToRank('',";
			if (j==exType){itrHTML += "-1);return false;\"><img src=\"/img/minus.png\">";}else{itrHTML += "2);return false;\"><img src=\"/img/plus.png\">";}
			itrHTML += " <b>Income and Careers</b></a></div>";
		}
		if (j==3){
			itrHTML += "<div style=\"padding-left:10px;\"><a href=\"#\" onClick=\"loadItemsToRank('',";
			if (j==exType){itrHTML += "-1);return false;\"><img src=\"/img/minus.png\">";}else{itrHTML += "3);return false;\"><img src=\"/img/plus.png\">";}
			itrHTML += " <b>Housing</b></a></div>";
		}
		if (j==4){
			itrHTML += "<div style=\"padding-left:10px;\"><a href=\"#\" onClick=\"loadItemsToRank('',";
			if (j==exType){itrHTML += "-1);return false;\"><img src=\"/img/minus.png\">";}else{itrHTML += "4);return false;\"><img src=\"/img/plus.png\">";}
			itrHTML += " <b>Crime, Weather, etc.</b></a></div>";
		}
		//if ((j==0)||(j==exType)){
		if ((j==exType)){
			for (var i = 0; i < ra.length; i++) {
				if (ra[i][0]==j){
					if (itemToHighlight==ra[i][1]){
						itrHTML += "<span class=\"tpcitem\"><label for=\"itth"+ra[i][1]+"\"><input name=\"itth\" id=\"itth"+ra[i][1]+"\" value=\""+ra[i][1]+"\" type=\"radio\" checked><font color='red'>"+ra[i][2]+"</font></label></span>";
					}else{
						//itrHTML += "<span class=\"tpcitem\"><a href=\"#\" onClick=\"loadItemsToRank('"+ra[i][1]+"',-1);return false;\">"+ra[i][2]+"</a></span>";
						itrHTML += "<span class=\"tpcitem\"><label for=\"itth"+ra[i][1]+"\"><input name=\"itth\" id=\"itth"+ra[i][1]+"\" value=\""+ra[i][1]+"\" type=\"radio\" onClick=\"loadItemsToRank('"+ra[i][1]+"',-1);return false;\">"+ra[i][2]+"</label></span>";
					}
				}
			}
		}
	}
	popRankOf(rankOf);
	popDataDate(itemToHighlight);
	if (rankingDesc.length>0) itrHTML+='<div style="padding-left:10px;"><br><b>Selected Ranking Topic: <span style="background-color:yellow; color:#0077aa;">' + rankingDesc + '</span></b></div>';
	gObj("itemtorankcontent").innerHTML=itrHTML;
}
function popRankOf(scopeV){
	var frM = document.rsF.rankOf;
	scopeToplimit = 0;
	scopeBottomlimit=1000;
	scopeHighlight = 50;
	var currentVal=frM.value;
	if (gObj("withinloc").checked){
		rankWithin = document.rsF.rankWithin.value+"";
		if (rankWithin=="state"){
			scopeToplimit=10;
			if (rankOfChanged==0) currentVal='city';
		}
		if (rankWithin=="metro-area"){
			scopeToplimit=20;
			if (rankOfChanged==0) currentVal='city';
		}
		if (rankWithin=="county"){
			scopeToplimit=30;
			if (rankOfChanged==0) currentVal='city';
		}
		if (rankWithin=="city"){
			scopeToplimit=50;
			if (rankOfChanged==0) currentVal='census-tract';
		}
		if (rankWithin=="school-district"){
			scopeToplimit=40;
			if (rankOfChanged==0) currentVal='census-tract';
		}
		if (rankWithin=="zip-code"){
			scopeToplimit=70;
			if (rankOfChanged==0) currentVal='block-group';
		}
		if (rankWithin=="us"){
			scopeBottomlimit=71;
			if (rankOfChanged==0) currentVal='state';
		}
		//if (rankWithin=="state") scopeBottomlimit=82;
	}

	frM.length=0;
	i=0;
	scopeV = "//"+scopeV+"//";
	if (scopeV.length<6) scopeV="//10/20/30/50/60/70/80/81/82//";
	selectid=-1;
	if ((scopeV.indexOf("/10/")>0)&&(scopeToplimit<10)){
		if (currentVal=='state') selectid=i;
		frM.options[i++] = new Option('State','state');
	}
	if ((scopeV.indexOf("/20/")>0)&&(scopeToplimit<20)){
		if (currentVal=='metro-area') selectid=i;
		frM.options[i++] = new Option('Metro Area','metro-area');
	}
	if ((scopeV.indexOf("/30/")>0)&&(scopeToplimit<30)){
		if (currentVal=='county') selectid=i;
		frM.options[i++] = new Option('County','county');
	}
	if ((scopeV.indexOf("/50/")>0)&&(scopeToplimit<50)){
		if (currentVal=='city') selectid=i;
		frM.options[i++] = new Option('City','city');
	}
	if ((scopeV.indexOf("/60/")>0)&&(scopeToplimit<60)){
		if (currentVal=='school-district') selectid=i;
		frM.options[i++] = new Option('School District','school-district');
	}
	if ((scopeV.indexOf("/70/")>0)&&(scopeToplimit<70)){
		if (currentVal=='zip-code') selectid=i;
		frM.options[i++] = new Option('Zip','zip-code');
	}
	if ((scopeV.indexOf("/80/")>0)&&(scopeBottomlimit>80)){
		if (currentVal=='census-tract') selectid=i;
		frM.options[i++] = new Option('Census Tract','census-tract');
	}
	if ((scopeV.indexOf("/81/")>0)&&(scopeBottomlimit>81)){
		if (currentVal=='block-group') selectid=i;
		frM.options[i++] = new Option('Census Block Group','block-group');
	}
	if ((scopeV.indexOf("/82/")>0)&&(scopeBottomlimit>82)){
		if (currentVal=='block') selectid=i;
		frM.options[i++] = new Option('Census Block','block');
	}
	if (selectid>-1){
		frM.options[selectid].selected = true;
	}
	/*
	if (i<1){
		frM.options[i++] = new Option('State','state');
		if (scopeToplimit<20) frM.options[i++] = new Option('Metro Area','metro-area');
		if (scopeToplimit<30) frM.options[i++] = new Option('County','county');
		if (scopeToplimit<50) frM.options[i++] = new Option('City','city');
		if (scopeToplimit<60) frM.options[i++] = new Option('School District','school-district');
		if (scopeToplimit<70) frM.options[i++] = new Option('Zip','zip-code');
		frM.options[i++] = new Option('Census Tract','census-tract');
		frM.options[i++] = new Option('Census Block Group','block-group');
		frM.options[i++] = new Option('Census Block','block');
	}
	*/
}
function popDataDate(itemToRank){
	var frM = document.rsF.datadate;
	currentVal = document.rsF.datadate.value;
	if (itemToRank.length>0){
		for (var i = 0; i < ra.length; i++) {
			if (ra[i][1]==itemToRank) dType = ra[i][0];
		}
	}
	dType=0;
	if ((dType==4)||("population-density"==itemToRank)||("public-school-performance"==itemToRank)||("crime-index"==itemToRank)||("average-temperature"==itemToRank)){
		gObj("datadateDiv").style.display = 'none';
	}else{
		gObj("datadateDiv").style.display = 'block';
		has9000=0;
		has8000=0;
		has6000=0;
		has3000=0;
		has2000=0;
		has1000=0;
		if ('total-population'==itemToRank||'white-population-percentage'==itemToRank||'black-population-percentage'==itemToRank||'asian-population-percentage'==itemToRank||'native-population-percentage'==itemToRank||'other-one-race-population-percentage'==itemToRank||'two-or-more-race-population-percentage'==itemToRank||'hispanic-population-percentage'==itemToRank||'male-population-percentage'==itemToRank||'female-population-percentage'==itemToRank||'percentage-of-population-age-5-or-less'==itemToRank||'percentage-of-population-age-5-to-9'==itemToRank||'percentage-of-population-age-10-to-14'==itemToRank||'percentage-of-population-age-15-to-19'==itemToRank||'percentage-of-population-age-20-to-24'==itemToRank||'percentage-of-population-age-25-to-34'==itemToRank||'percentage-of-population-age-35-to-44'==itemToRank||'percentage-of-population-age-45-to-54'==itemToRank||'percentage-of-population-age-55-to-64'==itemToRank||'percentage-of-population-age-65-to-74'==itemToRank||'percentage-of-population-age-75-to-84'==itemToRank||'percentage-of-population-age-85-or-older'==itemToRank||'median-age'==itemToRank||'male-median-age'==itemToRank||'female-median-age'==itemToRank||'total-household'==itemToRank||'average-household-size'==itemToRank||'total-family'==itemToRank||'average-family-size'==itemToRank||'one-person-household-percentage'==itemToRank||'two-or-more-people-household-percentage'==itemToRank||'family-household-percentage'==itemToRank||'married-couple-family-household-percentage'==itemToRank||'nonfamily-household-percentage'==itemToRank||'total-housing-units'==itemToRank||'occupied-housing-units-percentage'==itemToRank||'owner-occupied-housing-units-percentage'==itemToRank||'renter-occupied-housing-units-percentage'==itemToRank||'vacant-housing-units-percentage'==itemToRank||'vacant-for-rent-housing-units-percentage'==itemToRank||'vacant-for-sale-only-housing-units-percentage'==itemToRank||'rented-or-sold-not-occupied-housing-units-percentage'==itemToRank||'vacant-for-seasonal-recreational-or-occasional-use-housing-units-percentage'==itemToRank||'vacant-for-migrant-workers-housing-units-percentage'==itemToRank||'other-vacant-housing-units-percentage'==itemToRank) has8000=1;
		rankOf = document.rsF.rankOf.value;
		if (rankOf.length>0){
			if ('state'==rankOf){
				has9000=1;
				has6000=1;
				has3000=1;
				has2000=1;
				has1000=1;
			}else if ('metro-area'==rankOf){
				has9000=1;
				has6000=1;
				has3000=1;
				has2000=1;
			}else if ('county'==rankOf){
				has9000=1;
				has6000=1;
				has3000=1;
				has2000=1;
				has1000=1;
			}else if ('city'==rankOf){
				has9000=1;
				has6000=1;
				has3000=1;
				has2000=1;
				has1000=1;
			}else if ('school-district'==rankOf){
				has9000=1;
				has6000=1;
				has3000=1;
				has2000=1;
			}else if ('zip-code'==rankOf){
				has9000=1;
				has1000=1;
			}else if ('census-tract'==rankOf){
				has9000=1;
				has3000=1;
			}else if ('block-group'==rankOf){
				has9000=1;
				has3000=1;
			}
		}else{
			has9000=1;
			has6000=1;
			has3000=1;
			has2000=1;
			has1000=1;
		}
		totalItems = has9000+has8000+has6000+has3000+has2000+has1000;
		frM.length=0;
		i=0;
		selectid=-1;
		if (totalItems==0){
			alert("Sorry we do not have data for this ranking!");
		}else{
			if (has9000==1){
				if (currentVal=='9000') selectid=i;
				frM.options[i++] = new Option('American Community Survey 2010-2014','9000');
			}
			if (has8000==1){
				if (currentVal=='8000') selectid=i;
				frM.options[i++] = new Option('U.S. Census 2010','8000');
			}
			if (has6000==1){
				if (currentVal=='6000') selectid=i;
				frM.options[i++] = new Option('American Community Survey 2008-2012','6000');
			}
			if (has3000==1){
				if (currentVal=='3000') selectid=i;
				frM.options[i++] = new Option('American Community Survey 2006-2010','3000');
			}
			if (has2000==1){
				if (currentVal=='2000') selectid=i;
				frM.options[i++] = new Option('American Community Survey 2005-2009','2000');
			}
			if (has1000==1){
				if (currentVal=='1000') selectid=i;
				frM.options[i++] = new Option('U.S. Census 2000','1000');
			}
			if (selectid>-1) frM.options[selectid].selected = true;
			if (totalItems==1){
				frM.options[0].selected = true;
				gObj("datadateDiv").style.display = 'none';
			}
		}
	}
}
function changeScope(inType,inStateV){
	var frM = document.rsF;
	rankWithin = frM.rankWithin.value+"";
	if (inType==1){
		if (rankWithin.length>0){
			if (rankWithin=='us'){
				gObj("rankWithin2").style.display = 'none';
				gObj("rankWithin3").style.display = 'none';
			}else if (rankWithin=='metro-area'){
				gObj("rankWithin2").style.display = 'block';
				gObj("rankWithin3").style.display = 'none';
				loadXMLDoc("/rank/getGeo.php?t=metro-area");
			}else{
				gObj("rankWithin2").style.display = 'block';
				gObj("rankWithin3").style.display = 'none';
				loadXMLDoc("/rank/getGeo.php?t=state");
			}
		}
		itrHTML=gObj("itemtorankcontent").innerHTML;
		loadItemsToRank(frM.itemtorank.value, -1);
		gObj("itemtorankcontent").innerHTML=itrHTML;
	}else{
		gObj("rankWithin2").style.display = 'block';
		gObj("rankWithin3").style.display = 'block';
		geoStateV = inStateV+"";
		if (geoStateV.length<2) geoStateV=frM.rankWithin2.value;
		if (rankWithin=='county'){
			loadXMLDoc("/rank/getGeo.php?t=county&state="+geoStateV);
		}else if (rankWithin=='city'){
			loadXMLDoc("/rank/getGeo.php?t=city&state="+geoStateV);
		}else if (rankWithin=='school-district'){
			loadXMLDoc("/rank/getGeo.php?t=school-district&state="+geoStateV);
		}else if (rankWithin=='zip-code'){
			loadXMLDoc("/rank/getGeo.php?t=zip&state="+geoStateV);
		}else{
			gObj("rankWithin3").style.display = 'none';
		}
	}
}
function populateScopeState(inStateV){
	gObj("rankWithin2").style.display = 'block';
	gObj("rankWithin3").style.display = 'none';
	var frM = document.rsF.rankWithin2;
	i=0
	selectid=-1;
	frM.length=0;
	frM.options[i++]= new Option("---Please Select---","");
	var resArray=("10||alabama-state|Alabama||alaska-state|Alaska||arizona-state|Arizona||arkansas-state|Arkansas||california-state|California||colorado-state|Colorado||connecticut-state|Connecticut||delaware-state|Delaware||district-of-columbia-state|District of Columbia||florida-state|Florida||georgia-state|Georgia||hawaii-state|Hawaii||idaho-state|Idaho||illinois-state|Illinois||indiana-state|Indiana||iowa-state|Iowa||kansas-state|Kansas||kentucky-state|Kentucky||louisiana-state|Louisiana||maine-state|Maine||maryland-state|Maryland||massachusetts-state|Massachusetts||michigan-state|Michigan||minnesota-state|Minnesota||mississippi-state|Mississippi||missouri-state|Missouri||montana-state|Montana||nebraska-state|Nebraska||nevada-state|Nevada||new-hampshire-state|New Hampshire||new-jersey-state|New Jersey||new-mexico-state|New Mexico||new-york-state|New York||north-carolina-state|North Carolina||north-dakota-state|North Dakota||ohio-state|Ohio||oklahoma-state|Oklahoma||oregon-state|Oregon||pennsylvania-state|Pennsylvania||rhode-island-state|Rhode Island||south-carolina-state|South Carolina||south-dakota-state|South Dakota||tennessee-state|Tennessee||texas-state|Texas||utah-state|Utah||vermont-state|Vermont||virginia-state|Virginia||washington-state|Washington||west-virginia-state|West Virginia||wisconsin-state|Wisconsin||wyoming-state|Wyoming").split("||");
	for (var j = 1; j < resArray.length; j++) {
		if (inStateV==resArray[j].substring(0, resArray[j].indexOf("|"))) selectid=j;
		frM.options[i++] = new Option(resArray[j].substring(resArray[j].indexOf("|")+1),resArray[j].substring(0, resArray[j].indexOf("|")));
	}
	if (selectid>0){
		frM.options[selectid].selected = true;
	}
}
function ajaxOperation(response){
	var frM = document.rsF;
	var resArray=(response+"").split("||");
	i=0
	selectid=-1;
	var frMs=frM.rankWithin2;
	if ((resArray[0]=='10')||(resArray[0]=='20')){
		frM.rankWithin2.length=0;
		frM.rankWithin2.options[i++]= new Option("---Please Select---","");
		for (var j = 1; j < resArray.length; j++) {
			if (highlightScope1==resArray[j].substring(0, resArray[j].indexOf("|"))){
				highlightScope1="";
				selectid=j;
			}
			frM.rankWithin2.options[i++] = new Option(resArray[j].substring(resArray[j].indexOf("|")+1),resArray[j].substring(0, resArray[j].indexOf("|")));
		}
	}else if ((resArray[0]=='30')||(resArray[0]=='50')||(resArray[0]=='60')||(resArray[0]=='70')){
		var frMs=frM.rankWithin3;
		frM.rankWithin3.length=0;
		frM.rankWithin3.options[i++]= new Option("---Please Select---","");
		for (var j = 1; j < resArray.length; j++) {
			if (highlightScope2==resArray[j].substring(0, resArray[j].indexOf("|"))){
				highlightScope2="";
				selectid=j;
			}
			frM.rankWithin3.options[i++] = new Option(resArray[j].substring(resArray[j].indexOf("|")+1),resArray[j].substring(0, resArray[j].indexOf("|")));
		}
	}
	if (selectid>0){
		frMs.options[selectid].selected = true;
	}
}
function switchScope(scopeSelect){
	if (scopeSelect=='1'){
		gObj("withinPointDiv").style.display = 'none';
		gObj("withinLocationDiv").style.display = 'block';
		gObj("limitToStateDiv").style.display = 'none';
	}else{
		gObj("withinPointDiv").style.display = 'block';
		gObj("withinLocationDiv").style.display = 'none';
		gObj("limitToStateDiv").style.display = 'block';
	}
}
function sendForm(){
	gObj("itemtorankerror").innerHTML = "";
	gObj("rankingscopeerror").innerHTML = "";
	gObj("rankoferror").innerHTML = "";

	var frM = document.rsF;
	var toURL="";
	itemtorank = frM.itemtorank.value+"";
	rankingscope = "loc";
	if (gObj("withinpoint").checked) rankingscope = "dis";
	rankWithin = frM.rankWithin.value+"";
	rankWithin2 = frM.rankWithin2.value+"";
	rankWithin3 = frM.rankWithin3.value+"";
	pointDis = trimAll(frM.ml.value+"");
	pointCenter = trimAll(frM.lc.value+"");

	rankOf = frM.rankOf.value+"";
	datadate = frM.datadate.value+"";
	limitToState = frM.limitToState.value+"";
	poplow = trimAll(frM.poplow.value+"");
	pophigh = trimAll(frM.pophigh.value+"");

	hasError = 0;
	if (itemtorank.length<2){
		hasError = 1;
		gObj("itemtorankerror").innerHTML = "<font color='red'>Please select a topic.</font><br>";
	}
	if ("loc"==rankingscope){
		if (rankWithin.length<1){
			hasError = 1;
			gObj("rankingscopeerror").innerHTML = "<font color='red'>Please select ranking scope location.</font><br>";
		}else{
			if (rankWithin!="us"){
				if (rankWithin2.length<1){
					hasError = 1;
					gObj("rankingscopeerror").innerHTML = "<font color='red'>Please select a location.</font><br>";
				}else{
					if ((rankingscope=="county")&&(rankWithin3.length<1)){
						hasError = 1;
						gObj("rankingscopeerror").innerHTML = "<font color='red'>Please select a county.</font><br>";
					}
					if ((rankingscope=="city")&&(rankWithin3.length<1)){
						hasError = 1;
						gObj("rankingscopeerror").innerHTML = "<font color='red'>Please select a city.</font><br>";
					}
					if ((rankingscope=="school-district")&&(rankWithin3.length<1)){
						hasError = 1;
						gObj("rankingscopeerror").innerHTML = "<font color='red'>Please select a school district.</font><br>";
					}
					if ((rankingscope=="zip-code")&&(rankWithin3.length<1)){
						hasError = 1;
						gObj("rankingscopeerror").innerHTML = "<font color='red'>Please select a zip code.</font><br>";
					}
				}
			}
		}
	}else{
		if ((pointDis.length<1)||(!isNumber(pointDis))){
			hasError = 1;
			gObj("rankingscopeerror").innerHTML = "<font color='red'>Please provide a valid distance number.</font><br>";
		}
		if (pointCenter.length<1){
			hasError = 1;
			gObj("rankingscopeerror").innerHTML = "<font color='red'>Please provide a valid location center.</font><br>";
		}
	}
	if (rankOf.length<1){
		hasError = 1;
		gObj("rankoferror").innerHTML = "<font color='red'>Please select location type to rank.</font><br>";
	}
	if (hasError==0){
		if ("loc"==rankingscope){
			if (rankWithin=='us'){
				toURL = 'us';
			}else if (rankWithin=='state'||rankWithin=='metro-area'){
				toURL = rankWithin2;
			}else{
				toURL = rankWithin3;
			}
			toURL += "--"+itemtorank+"--"+rankOf+"-rank.htm?";
		}else{
			toURL = itemtorank+"--rank-of-"+rankOf+"-near--"+pointCenter+".htm?";
		}
		toURL+="yr="+datadate+"&dis="+pointDis+"&wist="+limitToState+"&plow="+poplow+"&phigh="+pophigh;
		window.location.href=toURL;
	}
}