package runci;

import sys.io.*;
using StringTools;

class Indexer
{
	static function main():Void {
		switch (Sys.args()) {
			case []:
				throw "missing s3 path argument";
			case [s3path]:
				index(s3path);
		}
	}

	public static function index(s3path:String)
	{
		var spaceRegex = ~/[ \t]+/g,
				s3pathRegex = ~/s3:\/\/([^\/]+)/;
		if (!s3path.endsWith('/')) {
			s3path = '$s3path/';
		}

		if (!s3pathRegex.match(s3path)) {
			throw 'Invalid s3 path $s3path';
		}

		var basePath = 'http://' + s3pathRegex.matched(1) + '.s3-website-us-east-1.amazonaws.com' + s3pathRegex.matchedRight();
		var proc = new Process('aws',['s3','ls',s3path,'--region','us-east-1']);
		var records = [],
				dirs = [];
		try
		{
			var i = proc.stdout;
			while(true)
			{
				var ln = spaceRegex.split(i.readLine());
				// trace(ln);

				inline function getPath(path:String)
				{
					return basePath + path;
				}
				switch(ln[1])
				{
					case 'PRE':
						dirs.push(getPath(ln[2]));
					case _:
						var size = ln[2];
						var path = ln[3];
						path = getPath(path);
						records.push({ date: ln[0] + ' ' + ln[1], size: ln[2], path: path, fname:haxe.io.Path.withoutDirectory(path) });
				}
			}
		}
		catch(e:haxe.io.Eof) {}

		var maxSizes = { date:25, size:15, fname:0 };
		for (r in records)
		{
			if (r.date.length > maxSizes.date)
				maxSizes.date = r.date.length;
			if (r.size.length > maxSizes.size)
				maxSizes.size = r.size.length;
			if (r.fname.length > maxSizes.fname)
				maxSizes.fname = r.fname.length;
		}
		records.sort(function(v1,v2) return Reflect.compare(v2.date,v1.date));
		var index = sys.io.File.write('index.html');

		index.writeString(
'
<html>
<head>
<title>Haxe git builds</title>
</head>
<body>
<!-- Google Tag Manager -->
<noscript><iframe src="//www.googletagmanager.com/ns.html?id=GTM-M4JZKD"
height="0" width="0" style="display:none;visibility:hidden"></iframe></noscript>
<script>(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({"gtm.start":
	new Date().getTime(),event:"gtm.js"});var f=d.getElementsByTagName(s)[0],
	j=d.createElement(s),dl=l!="dataLayer"?"&l="+l:"";j.async=true;j.src=
	"//www.googletagmanager.com/gtm.js?id="+i+dl;f.parentNode.insertBefore(j,f);
})(window,document,"script","dataLayer","GTM-M4JZKD");</script>
<!-- End Google Tag Manager -->
	<div id="listing">
	<pre>
');

		inline function data(date:String,size:String,key:String,path:String)
		{
			index.writeString(date.rpad(' ',maxSizes.date));
			index.writeString(size.rpad(' ',maxSizes.size));
			if (path != null && path != '')
				index.writeString('<a href="$path">$key</a>\n');
			else
				index.writeString('$key\n');
		}

		data('Last Modified', 'Size', 'Path','');
		for (i in 0...(maxSizes.date + maxSizes.size + maxSizes.fname + 5))
			index.writeString('-');
		index.writeString('\n\n');

		for (dir in dirs)
			data('','DIR',haxe.io.Path.withoutDirectory(dir.substr(0,dir.length-1)),dir);
		for (r in records)
			if (r.fname != 'index.html')
				data(r.date,r.size,r.fname,r.path);

		index.writeString(
'
</pre>
</div>
</body>
</html>
');

		index.close();
	}
}
