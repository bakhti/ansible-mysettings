// -*- Mode: js -*-
/// WebJumps

define_webjump("pocket-save", "javascript:(function(){var%20e=function(t,n,r,i,s){var%20o=[3962326,5509432,9989417,5137421,6478356,6013421,7784509,1321833,4633230,5599450];var%20i=i||0,u=0,n=n||[],r=r||0,s=s||0;var%20a={'a':97,'b':98,'c':99,'d':100,'e':101,'f':102,'g':103,'h':104,'i':105,'j':106,'k':107,'l':108,'m':109,'n':110,'o':111,'p':112,'q':113,'r':114,'s':115,'t':116,'u':117,'v':118,'w':119,'x':120,'y':121,'z':122,'A':65,'B':66,'C':67,'D':68,'E':69,'F':70,'G':71,'H':72,'I':73,'J':74,'K':75,'L':76,'M':77,'N':78,'O':79,'P':80,'Q':81,'R':82,'S':83,'T':84,'U':85,'V':86,'W':87,'X':88,'Y':89,'Z':90,'0':48,'1':49,'2':50,'3':51,'4':52,'5':53,'6':54,'7':55,'8':56,'9':57,'\/':47,':':58,'?':63,'=':61,'-':45,'_':95,'&':38,'$':36,'!':33,'.':46};if(!s||s==0){t=o[0]+t}for(var%20f=0;f<t.length;f++){var%20l=function(e,t){return%20a[e[t]]?a[e[t]]:e.charCodeAt(t)}(t,f);if(!l*1)l=3;var%20c=l*(o[i]+l*o[u%o.length]);n[r]=(n[r]?n[r]+c:c)+s+u;var%20p=c%(50*1);if(n[p]){var%20d=n[r];n[r]=n[p];n[p]=d}u+=c;r=r==50?0:r+1;i=i==o.length-1?0:i+1}if(s==206){var%20v='';for(var%20f=0;f<n.length;f++){v+=String.fromCharCode(n[f]%(25*1)+97)}o=function(){};return%20v+'c41b4fae79'}else{return%20e(u+'',n,r,i,s+1)}};var%20t=document,n=t.location.href,r=t.title;var%20i=e(n);var%20s=t.createElement('script');s.type='text/javascript';s.src='https://getpocket.com/b/r4.js?h='+i+'&u='+encodeURIComponent(n)+'&t='+encodeURIComponent(r);e=i=function(){};var%20o=t.getElementsByTagName('head')[0]||t.documentElement;o.appendChild(s)})()")
define_webjump("pocket-open", "http://getpocket.com/a/queue/")
define_webjump("intodns", "http://www.intodns.com/%s");
define_webjump("robtex", "http://www.robtex.com/dns/%s.html#records");
define_webjump("internic", "http://reports.internic.net/cgi/whois?whois_nic=%s&type=nameserver");
define_webjump("sslshopper", "http://www.sslshopper.com/ssl-checker.html?hostname=%s");
define_webjump("lingvo", "http://slovari.yandex.ru/%s/ru-en/#lingvo/");
define_webjump("ping", "http://just-ping.com/index.php?vh=%s&s=ping");
define_webjump("pinterest", "javascript:void((function(){var%20e=document.createElement('script');e.setAttribute('type','text/javascript');e.setAttribute('charset','UTF-8');e.setAttribute('src','http://assets.pinterest.com/js/pinmarklet.js?r='+Math.random()*99999999);document.body.appendChild(e)})());");
define_webjump("mxbl", "http://www.mxtoolbox.com/SuperTool.aspx?action=blacklist:%s");
define_webjump("mahd", "http://mahou.org/Dict/?word=%s&d=All");
define_webjump("mahk", "http://mahou.org/Kanji/?k=%s");
define_webjump("weboptim", "http://analyze.websiteoptimization.com/authenticate.php?url=%s");
define_webjump("babla", "http://en.bab.la/dictionary/japanese-english/%s");
define_webjump("kanjidict", "http://sp.cis.iwate-u.ac.jp/icampus/u/akanji.jsp?k=%s");
define_webjump("karad", "http://jisho.org/kanji/radicals/");
define_webjump("gter", "http://translate.google.com/#en|ru|%s");
define_webjump("gtre", "http://translate.google.com/#ru|en|%s");
define_webjump("orglist", "http://search.gmane.org/?query=%s&group=gmane.emacs.orgmode");
define_webjump("worg", "http://www.google.com/cse?cx=002987994228320350715%3Az4glpcrritm&ie=UTF-8&q=%s&sa=Search&siteurl=orgmode.org%2Fworg%2F");
define_webjump("squish", "http://www.squish.net/dnscheck/dnscheck.cgi?host=%s&type=ANY");
define_webjump("ssllabs", "https://www.ssllabs.com/ssldb/analyze.html?d=%s");
define_webjump("ntools", "http://network-tools.com/default.asp?prog=dnsrec&host=%s");
define_webjump("e2j",
	       function (term) {
		   return load_spec(
		       { uri: "http://www.freedict.com/onldict/onldict.php",
			 post_data: make_post_data([['search', term], ['exact', 'true'], ['selected', '10'],
						    ['from', 'English'], ['to', 'Japanese'],
						    ['fname', 'eng2jap1'], ['back', 'jap.html']]) });
	       },
	       $alternative = "http://www.freedict.com/onldict/jap.html",
	       $argument = 'optional');
define_webjump("wtb", "http://www.worldtimebuddy.com/");
