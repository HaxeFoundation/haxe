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

package mtwin.templo;

class Macro {

	static var R_PARAMS = ~/^([a-zA-Z0-9_]+)\((.*?)\)$/g;
	static var R_NOTVAR = ~/[^a-zA-Z0-9_.]/;
	static var R_NUM    = ~/^[0-9.]+$/;
	static var R_VAR    = ~/^::(.*?)::$/;

	public var name : String;
	var params : Array<String>;
	var source : String;

	public function new( n:String, c:String ){
		params = new Array();
		name = n;
		if (R_PARAMS.match(name)){
			name = R_PARAMS.matched(1);	
			if (StringTools.trim(R_PARAMS.matched(2)) != ""){
				var i = 0;
				for (param in R_PARAMS.matched(2).split(","))
					params[i++] = StringTools.trim(param);
			}
		}
		source = c;
	}

	dynamic public static function debug(x:String){}

	public function expand( p:Array<String> ) : String {
		if (params.length != p.length){
			throw "macro "+name+" takes "+params.length+" arguments"+params.toString()+"\ngot: "+p.toString();
		}

		var hashReplace = new Hash();
		
		var res = source;
		for (i in 0...params.length){
			// Extract raw parameters ::myparam:: for later replacement,
			// this is faster than trying to parse simple raw replacement like ::myparam::,
			// an additional benefit is that this little replacement prevent the macro system from 
			// becoming mad when a call parameter references a variable with the same name as one 
			// of the macro arguments name (ex: macro(content) $$macro(::content::foo bar baz))
			var replace = new EReg("::\\s*?"+params[i]+"\\s*?::", "g");
			var key = "##__MP__"+i+"__##";
			hashReplace.set(key, p[i]);	
			res = replace.replace(res, key);
			// old
			// res = replace.replace(res, StringTools.replace(p[i], "$", "$$"));

			// work on complex expressions ::myparam + 10 + 'foo bar baz'::
			var isNum = R_NUM.match(p[i]);
			var isVar = (R_VAR.match(p[i]) && R_VAR.matched(1).indexOf("::") == -1);
			var pos = res.indexOf("::", 0);
			while (pos != -1){
				var end = res.indexOf("::", pos+2);
				if (end == null){
					neko.Lib.print(res);
					throw "Unable to find matching ::";
				}
				var exp = res.substr(pos+2, end-pos-2);
				var rep = replaceArgumentInExpression(exp, params[i], p[i], isVar, isNum);
				res = res.substr(0, pos+2) + rep + res.substr(end, res.length - end);
				pos = end - exp.length + rep.length + 2;
				pos = res.indexOf("::", pos);
			}
			// work on mt:*="" attributes
			var param = params[i];
			var paramValue = p[i];
			var reg = ~/(mt:[a-z-]+=)(["'])(.*?)(\2)/sm;
			res = reg.customReplace(res, function(r){
				return r.matched(1) + r.matched(2) + replaceArgumentInExpression(r.matched(3), param, paramValue, isVar, isNum) + r.matched(4);
			});
		}

		// Replace raw parameters
		for (k in hashReplace.keys())
			res = StringTools.replace(res, k, hashReplace.get(k));
		return res;
	}

	static function replaceArgumentInExpression( exp:String, paramName:String, value:String, isVar:Bool, isNum:Bool ) : String {	
		var repl = if (isNum) value 
			else if (isVar) "(" + value.substr(2, value.length-4) + ")" 
			else stringArgumentToExpressionCompound(value);
		var res = exp;
		var pos = res.indexOf(paramName, 0);
		while (pos != -1){
			var end = pos + paramName.length;
			var before = if (pos > 0) res.charAt(pos-1) else " ";
			var after = if (end < res.length) res.charAt(end) else " ";
			if (R_NOTVAR.match(before) && (after == "." || R_NOTVAR.match(after))){
				res = res.substr(0, pos) + repl + res.substr(end, res.length-end);
				end = end - paramName.length + repl.length;
			}
			pos = res.indexOf(paramName, end+1);
		}
		return res;
	}

	static function xmlToString( xml:Xml ) : String {
		if (xml.nodeType != Xml.Element){
			return xml.nodeValue;
		}
		var res = new StringBuf();
		res.add("<");
		res.add(xml.nodeName);
		for (i in xml.attributes()){
			res.add(" ");
			res.add(i);
			res.add("=\"");
			res.add(StringTools.htmlEscape(xml.get(i)));
			res.add("\"");
		}
		if (xml.firstChild() != null){
			res.add(">");
			for (x in xml)
				res.add(xmlToString(x));
			res.add("</");
			res.add(xml.nodeName);
			res.add(">");
		}
		else {
			res.add("/>");
		}
		return res.toString();
	}

	static function stringArgumentToExpressionCompound( str:String ) : String {
		var res = StringTools.replace(str,"'","\\'");
		var pos = res.indexOf("::",0);
		while ( pos != -1 ){
			var end = res.indexOf("::", pos+2);
			if (end == -1)
				throw "Malformed expression '"+str+"'";
			var left = res.substr(0, pos);
			var data = res.substr(pos+2, end-pos-2);
			var right = res.substr(end+2, res.length-end-2);
			res = left + "\'+" + data + "+\'" + right;
			pos = end + 2;
			pos = res.indexOf("::",pos);
		}
		return "(\'" + res + "\')";
	}
}
