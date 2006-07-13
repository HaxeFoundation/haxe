/*
 * Copyright (c) 2006, Motion-Twin
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY MOTION-TWIN "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

package mtwin.text;

/**
	Transform a plain text document into an XHTML document.

	h1 : at least 4 * after the title
	****

	or

	*** h1 : line starts with 3 *

	h2 : at least 4 = after the title
	====

	or

	=== h2 : line starts with 3 =

	h3 : at least 4 - after the title
	----
	
	or 

	--- h3 : line starts with 3 -

	- list item 1
	- list item 2
    - no sub level at this time

	* ordered item 1
	* ordered item 2
	* no sub level at this time

	[pre]
	some preformatted code
	[/pre]

	Will generate a link : http://www.google.com.

	Will generate a [link with name:http://www.google.com].

	Some //emphased text// and some *strong text*.

	Now insert an image : @img http://www.foo.com/foo.jpg@

	Inserting a Swf is easy : @swf WxHxV http://path/to/my/swf.swf@ where W means width, H means Height and V means flash Version, 
	this feature uses the SWFObject javascript class to display the specified swf.

	[html]
	<p>This is some raw html you may like to insert manually to add some specific data like 
	javascript or other stuff.</p>
	<p>Raw html may produce parse exceptions thrown by the Text2Xhtml.transform(src) method.</p>
	[/html]

	You can also insert [cite]some citations[/cite] !	

	That's almost everything for now :)

	Ah i almost forgot the haxe colorizer :

	[haxe]
	class Foo {
		// some comment
		public function new(){
		}

		// ...

		public static function foo(){
		}
	}
	[/haxe]

**/
class Text2Xhtml {

	static var rh1 = ~/^\*\*\*\s+(.*?)$/gm;
	static var rh1e = ~/^(.*?)\n\*{4,}\s*?\n/gm;
	static var rh2 = ~/^===\s+(.*?)$/gm;
	static var rh2e = ~/^(.*?)\n={4,}\s*?\n/gm;
	static var rh3 = ~/^\-\-\-\s+(.*?)$/gm;
	static var rh3e = ~/^(.*?)\n-{4,}\s*?\n/gm;
	static var pre = ~/^\[pre\](.*?)\[\/pre\]/gsm;
	static var em = ~/(?<!http:)\/\/(.*?)(?<!http:)\/\//gsm;
	static var strong = ~/\*(.*?)\*/gm;
	static var link = ~/\[(.*?):http:\/\/(.*?)\]/g;
	static var http = ~/(?<!")http:\/\/(.*?)(\s|\.\s|$)/g; //"
	static var img = ~/@img (.*?)@/g;
	static var li = ~/\n\n(- (.*?))+\n\n/gsm;
	static var olli = ~/\n\n(\* (.*?))+\n\n/gsm;
	static var cite = ~/\[cite\](.*?)\[\/cite\]/gsm;
	static var html = ~/\[html\](.*?)\[\/html\]/gsm;
	static var haxe = ~/\[haxe\](.*?)\[\/haxe\]/gsm;
	static var swf = ~/@swf ([0-9]+)x([0-9]+)x([0-9]+) (http:\/\/.*?)@/g;

	public var paragraphSeparator : String;
	public var htmlEnabled : Bool;
	public var codeEnabled : Bool;
	public var swfEnabled : Bool;

	public function new(){
		paragraphSeparator = "\n\n";
		htmlEnabled = true;
		codeEnabled = true;
		swfEnabled = true;
	}

	public function transform( str:String ) : String {
		str = StringTools.htmlEscape(str);

		var helper = new StringHelper(str);
		var extractedPre = helper.extract("pre", pre, "<pre>$1</pre>");
		var extractedHtml = if (htmlEnabled) helper.extract("html", html, "$1") else new List();
		extractedHtml = extractedHtml.map(function(data:String){ return StringTools.htmlUnescape(data); });
		var extractedHaxe = if (codeEnabled) helper.extract("haxe", haxe, "$1") else new List();
		extractedHaxe = extractedHaxe.map(function(data:String){ return "<pre class=\"code haxe\">" + HaxeColorizer.colorize(data) + "</pre>"; });
		var swfs = if (swfEnabled) helper.extract("swf", swf, "$1@$2@$3@$4") else new List();
		swfs = swfs.map(swfProcess);

		str = helper.str;
		str = rh1.replace(str, "<h1>$1</h1>");
		str = rh1e.replace(str, "<h1>$1</h1>");
		str = rh2.replace(str, "<h2>$1</h2>");
		str = rh2e.replace(str, "<h2>$1</h2>");
		str = rh3.replace(str, "<h3>$1</h3>");
		str = rh3e.replace(str, "<h3>$1</h3>");

		while (olli.match(str)){
			var list = olli.matched(0);
			var result = new StringBuf();
			result.add("\n<ol>\n");
			var items = list.split("\n* ");
			for (i in 1...items.length){
				result.add("<li>"+StringTools.trim(items[i])+"</li>\n");
			}
			result.add("</ol>\n");
			str = StringTools.replace(str, list, result.toString());
		}

		while (li.match(str)){
			var list = li.matched(0);
			var result = new StringBuf();
			result.add("\n<ul>\n");
			var items = list.split("\n- ");
			for (i in 1...items.length){
				result.add("<li>"+StringTools.trim(items[i])+"</li>\n");
			}
			result.add("</ul>\n");
			str = StringTools.replace(str, list, result.toString());
		}
		
		var xml = Xml.parse(str);
		transformXml(xml);
		str = xml.toString();

		str = StringTools.replace(str, "\n\n", "\n");
		str = StringTools.replace(str, "\n\n", "\n");

		helper = new StringHelper(str);
		helper.restore("pre", extractedPre);
		helper.restore("html", extractedHtml);
		helper.restore("haxe", extractedHaxe);
		helper.restore("swf", swfs);
		str = helper.str;

		// cleanup
		str = StringTools.replace(str, "<p></p>", "");
		str = StringTools.replace(str, "><p>", ">\n<p>");

		// XML validation
		try {
			xml = Xml.parse(str);
		}
		catch (e:Dynamic){
			throw {error:Std.string(e), xml:str};
		}

		return str;
	}

	public static function transform( str:String ) : String {
		var transformer = new Text2Xhtml();
		return transformer.transform(str);
	}

	static function swfProcess(data:String){
		var s = Lambda.array(data.split("@").iterator());
		var id = Md5.encode(s[3]); // url
		var str = "<div id=\"swf@id\"></div>
<script type=\"text/javascript\">
//<![CDATA[
var s = new SWFObject('@url', 's@id', '@w', '@h', '@v', '#FFFFFF', true);
s.addParam('menu','false');
s.write('swf@id');
//]]>
</script>
";
		str = StringTools.replace(str, "@id", id);
		str = StringTools.replace(str, "@url", s[3]);
		str = StringTools.replace(str, "@v", s[2]);
		str = StringTools.replace(str, "@w", s[0]);
		str = StringTools.replace(str, "@h", s[1]);
		return str;
	}
	
	function transformXml( xml:Xml, ?noParagraph:Bool ) {
		if (xml.nodeType != Xml.Element && xml.nodeType != Xml.Document){
			var str = xml.nodeValue;
			if (noParagraph == null || noParagraph == false){
				var paragraphs = str.split(paragraphSeparator);
				var me = this;
				paragraphs = Lambda.amap(paragraphs, function(p){ return "<p>"+me.transformContent(StringTools.trim(p))+"</p>\n"; });
				str = paragraphs.join("\n");
			}
			else {
				str = transformContent(str);
			}
			xml.nodeValue = str;
		}
		if (xml.nodeType == Xml.Element && (xml.nodeName == "pre" || xml.nodeName == "t2x"))
			return;
		for (child in xml){
			transformXml(child, (xml.nodeType == Xml.Element && contains(SELF_CONTAINING_ELEMENTS.iterator(), xml.nodeName)));
		}
	}

	function transformContent( str:String ) : String {
		var helper = new StringHelper(str);
		var links = helper.extract("link", link, "<a href=\"http://$2\">$1</a>");
		var images = helper.extract("img", img, "<img src=\"$1\" alt=\"Image\"/>");
		var https = helper.extract("http", http, "<a href=\"http://$1\">http://$1</a>$2");
		str = helper.str;

		var pos = 0;
		var token = findFirst(str, [em, strong, cite]);
		while (token != null){
			var repl = switch (token.reg){
				case strong: "<strong>" + transformContent(token.reg.matched(1)) + "</strong>";
				case em: "<em>" + transformContent(token.reg.matched(1)) + "</em>";
				case cite: "<cite>" + transformContent(token.reg.matched(1)) + "</cite>";
			}
			var end = pos + token.pos.pos + token.pos.len;
			str = str.substr(0, pos+token.pos.pos) + repl + str.substr(end, str.length-end);
			pos = pos + repl.length;
			token = findFirst(str.substr(pos, str.length-pos), [em,strong]);
		}
		
		helper = new StringHelper(str);
		helper.restore("link", links);
		helper.restore("img", images);
		helper.restore("http", https);
		return helper.str;
	}

	static function findFirst( str:String, regs:Array<EReg> ) : {reg:EReg, pos:{pos:Int, len:Int}} {
		var min : {reg:EReg, pos:{pos:Int, len:Int}} = {reg:null, pos:null};
		for (reg in regs){
			if (reg.match(str)){
				var pos : {pos:Int, len:Int} = reg.matchedPos();
				if (min.pos == null || pos.pos < min.pos.pos){
					min = {reg:reg, pos:pos};
				}
			}
		}
		if (min.pos == null)
			return null;
		return min;
	}

	static function contains( i:Iterator<Dynamic>, v:Dynamic ){
		for (x in i)
			if (x == v) return true;
		return false;
	}

	static var SELF_CONTAINING_ELEMENTS = ["pre","h1","h2","h3","h4","ul","li","cite"];
}

class StringHelper {
	
	public var str : String;

	public function new( s:String ){
		str = s;
	}

	public function extract( key:String, reg:EReg, replace:String ) : List<String> {
		var result = new List();
		while (reg.match(str)){
			var matched = reg.matched(0);
			result.push(reg.replace(matched, replace));
			str = StringTools.replace(str, matched, "<t2x>"+key+result.length+"</t2x>");
		}
		return result;
	}

	public function restore( key:String, list:List<String> ){
		var i = list.length;
		for (item in list){
			str = StringTools.replace(str, "<t2x>"+key+i+"</t2x>", item);
			--i;
		}
	}
}

class HaxeColorizer {
	static var lineComment = ~/(\/\/\s.*?)$/gm;
	static var comment = ~/(\/\*.*?\*\/)/gsm;
	static var string = ~/(?!<\\)(".*?(?!<\\)")/gsm;
	static var keywords = ~/(^|\s|[^a-zA-Z_0-9])(var|function|static|private|public|class|extends|typedef|signature|throw|extern|enum|in|interface|untyped|cast|this|new|try|catch|default|case|switch|import|continue|break|for|do|while|if|else|return)(\s|[^a-zA-Z_0-9])/gsm;

	public static function colorize( code:String ) : String {
		var helper = new StringHelper(code);
		var comments = helper.extract("comment", comment, "<span class=\"comment\">$1</span>");
		var strings = helper.extract("string", string, "<span class=\"string\">$1</span>");
		var lineComments = helper.extract("lc", lineComment, "<span class=\"comment\">$1</span>");
		helper.str = keywords.replace(helper.str, "$1<span class=\"keyword\">$2</span>$3");
		helper.restore("lc", lineComments);
		helper.restore("string", strings);
		helper.restore("comment", comments);
		return helper.str;
	}
}

