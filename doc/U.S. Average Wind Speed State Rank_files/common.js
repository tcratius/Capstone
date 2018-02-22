var ajaxreq;
//function loadXMLDocWithName(url, rtnname){if (window.XMLHttpRequest) {ajaxreq = new XMLHttpRequest();eval("ajaxreq.onreadystatechange = "+rtnname+";");ajaxreq.open("GET", url, true);ajaxreq.send(null);} else if (window.ActiveXObject) {ajaxreq = new ActiveXObject("Microsoft.XMLHTTP"); if (ajaxreq) {eval("ajaxreq.onreadystatechange = "+rtnname+";");ajaxreq.open("GET", url, true);ajaxreq.send();}}}
function loadXMLDocWithName(url, rtnname) {
	url = url + "";
	/*
	if (url.toLowerCase().substring(0, 4)!='http'){
		if (url.toLowerCase().substring(0, 1)=='/'){
			url = 'http://ajax.usa.com'+url;
		}else{
			url = 'http://ajax.usa.com/'+url;
		}
	}
	*/
	if (navigator.appName!='Microsoft Internet Explorer') {
		ajaxreq = new XMLHttpRequest();
		eval("ajaxreq.onreadystatechange = " + rtnname + ";");
		ajaxreq.open("GET", url, true);
		ajaxreq.send(null);
	} else if (window.ActiveXObject) {
		ajaxreq = new XDomainRequest();
		if (ajaxreq){
			eval("ajaxreq.onload = "+rtnname + ";");
			ajaxreq.open("GET", url);
			ajaxreq.send();
		}
	}
}
function loadXMLDoc(url){loadXMLDocWithName(url, "processAjaxReqChange");}
function processAjaxReqChange() {
	if ((navigator.appName=='Microsoft Internet Explorer')||((ajaxreq.readyState == 4)&&(ajaxreq.status == 200))){
		ajaxOperation(ajaxreq.responseText);
	}
}
function geoTypeAheadAjaxReqChange() {
	if ((navigator.appName=='Microsoft Internet Explorer')||((ajaxreq.readyState == 4)&&(ajaxreq.status == 200))){
		response = ajaxreq.responseText;
		if (response!=""){var adds=response.split("||"); if (adds.length > 0){ tyhwordlength = adds.length; tyhClearOutput(); for (var i=0;i < adds.length; ++i){ tyhAddWord(adds[i]); } tyhSetVisible("visible"); tyhinput = document.tsm.q.value;}else{ tyhSetVisible("hidden"); posi = -1; tyhwordlength=0; } }else{ tyhSetVisible("hidden"); posi = -1;tyhwordlength=0; }
	}
}
var tyhoutp; var tyholdins; var tyhposi = -1; var tyhinput; var tyhxoffset = 5; var tyhyoffset = -3; var tyhkey;var tyhwordlength=0;
function tyhSetVisible(visi){ var x = document.getElementById("tyhshadow"); var t = document.tsm.q; x.style.position = 'absolute'; x.style.top = (tyhFindPosY(t)+3)+"px"; x.style.left = (tyhFindPosX(t)+2)+"px"; x.style.visibility = visi; }
function tyhInit(){ tyhoutp = document.getElementById("tyhoutput"); window.setInterval("tyhLookAt()", 100); tyhSetVisible("hidden"); document.onkeydown = tyhkeygetter;document.onkeyup = tyhkeyHandler;document.onmouseup = tyhMouseClickAll;}
function tyhFindPosX(obj){ var curleft = 0; if (obj.offsetParent){ while (obj.offsetParent){ curleft += obj.offsetLeft; obj = obj.offsetParent; } }else if (obj.x) curleft += obj.x; return (curleft+tyhxoffset); }
function tyhFindPosY(obj){ var curtop = 0; if (obj.offsetParent){ curtop += obj.offsetHeight; while (obj.offsetParent){ curtop += obj.offsetTop; obj = obj.offsetParent; } }else if (obj.y){ curtop += obj.y; curtop += obj.height; } return (curtop+tyhyoffset); }
function tyhLookAt(){ var ins = document.tsm.q.value; if (tyholdins == ins) return; else if (tyhposi > -1); else if (ins.length > 0){ loadXMLDocWithName('/ajax-data/typeaheadgeo.php?k='+encodeURIComponent(ins), 'geoTypeAheadAjaxReqChange'); }else{ tyhSetVisible("hidden"); tyhposi = -1; } tyholdins = ins; }
function tyhAddWord(hints){ var hintarray=hints.split("|"); word = hintarray[0]+""; var parLoc=word.indexOf("("); surx = ""; if (parLoc>0){ surx = word.substr(parLoc); word = word.substr(0, parLoc); } var sp = document.createElement("div"); sp.appendChild(document.createTextNode(word)); if (parLoc>0){ var surxe = document.createElement("span"); surxe.appendChild(document.createTextNode(surx)); sp.appendChild(surxe); } sp.onmouseover = tyhMouseHandler; sp.onmouseout = tyhMouseHandlerOut; sp.onclick = tyhMouseClick; var newlink = document.createElement('a'); newlink.setAttribute('href', '/'+hintarray[1]); newlink.appendChild(sp); tyhoutp.appendChild(newlink); }
function tyhClearOutput(){ while (tyhoutp.hasChildNodes()){ noten=tyhoutp.firstChild; tyhoutp.removeChild(noten); } tyhposi = -1; }
var tyhMouseHandler=function(){ this.style.background = "blue"; this.style.color= "white"; }
var tyhMouseHandlerOut=function(){ this.style.background = "white"; this.style.color= "black"; }
var tyhMouseClick=function(){ document.tsm.q.value = this.firstChild.nodeValue; tyhSetVisible("hidden"); tyhposi = -1; tyholdins = this.firstChild.nodeValue; }
function tyhsetColor (_posi, _color, _forg){tyhoutp.childNodes[_posi].childNodes[0].style.background = _color;tyhoutp.childNodes[_posi].childNodes[0].style.color = _forg;}
function tyhkeygetter(event){if (!event && window.event) event = window.event; if (event) tyhkey = event.keyCode; else tyhkey = event.which;}
function tyhkeyHandler(event){if (document.getElementById("tyhshadow").style.visibility == "visible"){var textfield = document.tsm.q; if (tyhkey == 40){if (tyhwordlength > 0 && tyhposi <= (tyhwordlength-1)){if (tyhposi >=0) tyhsetColor(tyhposi, "#fff", "black"); else tyhinput = textfield.value;tyhsetColor(++tyhposi, "blue", "white");textfield.value = tyhoutp.childNodes[tyhposi].firstChild.firstChild.nodeValue;}}else if (tyhkey == 38){if (tyhwordlength > 0 && tyhposi >= 0){if (tyhposi >=1){tyhsetColor(tyhposi, "#fff", "black");tyhsetColor(--tyhposi, "blue", "white");textfield.value = tyhoutp.childNodes[tyhposi].firstChild.firstChild.nodeValue;}else{tyhsetColor(tyhposi, "#fff", "black");textfield.value = tyhinput;textfield.focus();tyhposi--;}}}else if (tyhkey == 27){textfield.value = tyhinput;tyhSetVisible("hidden");tyhposi = -1;tyholdins = tyhinput;}else if (tyhkey == 8){tyhposi = -1;tyholdins=-1;}}}
var tyhMouseClickAll=function(){tyhSetVisible("hidden");tyhposi = -1;}
function tyhOnSubmit(){if (tyhposi>0) return true; if ("hidden"==document.getElementById("tyhshadow").style.visibility){return true;}else{var hintVal = document.getElementById("tyhoutput").innerHTML+"";var hintLinkP = hintVal.indexOf("href=");if (hintLinkP>0){hintVal = hintVal.substr(hintLinkP+6);hintLinkV = hintVal.substr(0, hintVal.indexOf("\""))+"";if (hintLinkV.length>3){window.location = hintLinkV;return false;}}return true;}}

function gObj(obj){var theObj;if(document.all){if(typeof obj=="string"){return document.all(obj);}else{return obj.style;}}if(document.getElementById){if(typeof obj=="string"){return document.getElementById(obj);}else{return obj.style;}}return null;}
function trimAll(sString){while(sString.substring(0,1)==' '){sString=sString.substring(1,sString.length);}while(sString.substring(sString.length-1,sString.length)==' '){sString=sString.substring(0,sString.length-1);}return sString;}
function rsf(){var args=rsf.arguments;var frM=document.hsF;if(args.length>1){if(args[0]=='action'){eval("frM."+args[0]+"=args[1];");}else{eval("frM."+args[0]+".value=args[1];");}if(args.length>3)eval("frM."+args[2]+".value=args[3];");if(args.length>5)eval("frM."+args[4]+".value=args[5];");frM.submit();}}
function isNumber(val){val=val+"";if (val.length<1) return false;if (isNaN(val)){return false;}else{return true;}}
function getLocalDate(timeZone, stateName){
var isDST = 0;
d = new Date();var jan = new Date(d.getFullYear(), 0, 1);var nodstOffSet = jan.getTimezoneOffset();currentOffSet = d.getTimezoneOffset();if (nodstOffSet!=currentOffSet) isDST = 1;if ((stateName=='HI')||(stateName=='AZ')) isDST = 0;if (isDST==1) timeZone = timeZone+1;localTime = d.getTime();localOffset = d.getTimezoneOffset() * 60000;utc = localTime + localOffset;return new Date(utc + (3600000*timeZone));
}
function updateClock(timezone, stateName){dt = getLocalDate(timezone, stateName);weekNum = dt.getDay();weekDesc = '';if (weekNum==0) weekDesc=' Sunday';if (weekNum==1) weekDesc=' Monday';if (weekNum==2) weekDesc=' Tuesday';if (weekNum==3) weekDesc=' Wednesday';if (weekNum==4) weekDesc=' Thursday';if (weekNum==5) weekDesc=' Friday';if (weekNum==6) weekDesc=' Saturday';clockString = '<b>Local Time:</b> '+weekDesc+', '+(dt.getMonth()+1) + "/" + dt.getDate() + "/" + dt.getFullYear() + " " + dt.getHours()+":"+dt.getMinutes()+":"+dt.getSeconds();document.getElementById("clockValues").innerHTML = clockString;setTimeout("updateClock("+timezone+", '"+stateName+"')",1000);}

showid='';
function tabNav(inid){showid=inid;if ("tabexpo"==inid){gObj("tabexpe").style.display = 'none';gObj("tabexpo").style.display = 'block';}else if ("tabexpe"==inid){gObj("tabexpo").style.display = 'none';gObj("tabexpe").style.display = 'block';}else{gObj("tabexpo").style.display = 'none';gObj("tabexpe").style.display = 'none';}return false;}
function tabNavHideReal(){tabNav(showid);}
function tabNavHide(defid){showid=defid;setTimeout("tabNavHideReal()", 100);}
function searchinputprocess(status){infield = document.tsm.q;if (status==1){if (infield.value=='Search State, County, City, Zip Code, or Area Code'){infield.value='';infield.style.color="#000000";infield.style.fontStyle="normal";}}else{if (infield.value==''){infield.value='Search State, County, City, Zip Code, or Area Code';infield.style.color="#666666";infield.style.fontStyle="italic";}}}