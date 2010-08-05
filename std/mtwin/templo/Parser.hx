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

class Parser {

	static var REGEX_EXP = ~/^([a-zA-Z][A-Za-z0-9_]{0,})[ \n\r\t]+(.*?)$/gsm;
	static var REGEX_DXP = ~/::([^:].*?)::/gsm;

	static var MT = "mt";
	static var MT_IF = "mt:if";
	static var MT_ELSEIF = "mt:elseif";
	static var MT_ELSE = "mt:else";
	static var MT_CONTENT = "mt:content";
	static var MT_REPLACE = "mt:replace";
	static var MT_FOREACH = "mt:foreach";
	static var MT_ATTRIBUTES = "mt:attr";
	static var MT_SET = "mt:set";
	static var MT_FILL = "mt:fill";
	static var MT_OMIT_TAG = "mt:omit-tag";
	static var MT_MACRO = "mt:macro";
	static var MT_USE = "mt:use";

	static var XHTML_EMPTY = ["area","base","basefont","br","col","frame","hr","img","input","isindex","link","meta","param"];
	static var XHTML_ATTRS = ["compact","nowrap","ismap","declare","noshade","checked","disabled","readonly","multiple","selected","noresize","defer"];

	var out : mtwin.templo.Generator;
	var xhtmlMode : Bool;

	public function new( ?isXhtml:Bool ){
		xhtmlMode = (isXhtml == true);
		out = new mtwin.templo.Generator();
	}

	public function parse( xml:Xml ) : String {
		parseNode(xml);
		return out.toString();
	}

	function parseNode( xml:Xml ){
		switch (untyped xml.nodeType){
			case Xml.Document:
				for (child in xml) parseNode(child);
			case Xml.CData:
				parseCDATA(xml);
			case Xml.Comment:
				parseComment(xml);
			case Xml.Element:
				parseElement(xml);
			default:
				echoString(xml.toString());
		}
	}

	function parseElement( xml:Xml ){
		var mtSet = extractAttribute(xml, MT_SET);
		if (mtSet != null){
			var incExp = ~/^([a-zA-Z_][a-zA-Z0-9_]*?)\s*?([\+\-\/*%])=\s*?(.*?)$/;  // */
			if (incExp.match(mtSet)){
				var dest = StringTools.trim(incExp.matched(1));
				var op = incExp.matched(2);
				var exp = parseExpression(StringTools.trim(incExp.matched(3)));
				out.setVar(dest, out.getVar(dest)+" "+op+" ("+exp+")");
			}
			else {
				var parts = Lambda.array(mtSet.split("="));
				var dest = StringTools.trim(parts.shift());
				var exp = parseExpression(StringTools.trim(parts.join("=")));
				out.setVar(dest, "("+exp+")");
			}
			return;
		}

		var mtUse = extractAttribute(xml, MT_USE);
		if (mtUse != null){
			// ensure template is parsed (beware of cycle)
			/*
			if (!mtwin.templo.Template.compiledFiles.exists(mtUse)){
				mtwin.templo.Template.compiledFiles.set(mtUse, true);
				var f = mtwin.templo.Template.fromFile(mtUse);
			}
			*/
			out.add("tmp = "+out.getVar("__content__")+";\n");
			out.add("__out = new_output_buffer(__out);\n");
			parseNode(xml);
			out.setVar("__content__", "__out.str()");
			out.add("__out = __out.parent;\n");

			if (StringTools.endsWith(mtUse, ".mtt"))
				neko.Lib.println("WARNING: templo ::use:: changed the following use may be incorrect : '"+mtUse+"'");
			//throw parseExpression(mtUse);
			out.add("mcr = macro("+parseExpression(mtUse)+");\n");
			out.add("__out.add(mcr.template(macro, __ctx));\n");
			out.setVar("__content__", "tmp");
			return;
		}

		var mtFill = extractAttribute(xml, MT_FILL);
		if (mtFill != null){
			out.add("__out = new_output_buffer(__out);\n");
			parseNode(xml);
			out.setVar(StringTools.trim(mtFill), "String.new(__out.str())");
			out.add("__out = __out.parent;\n");
			return;
		}

		var mtIf = extractAttribute(xml, MT_IF);
		if (mtIf != null){
			out.add("if (is_true("+parseExpression(mtIf)+")){\n");
			parseNode(xml);
			out.add("}\n");
			return;
		}

		var mtElseIf = extractAttribute(xml, MT_ELSEIF);
		if (mtElseIf != null){
			out.add("else if (is_true("+parseExpression(mtElseIf)+")){\n");
			parseNode(xml);
			out.add("}\n");
			return;
		}

		var mtElse = extractAttribute(xml, MT_ELSE);
		if (mtElse != null){
			out.add("else {\n");
			parseNode(xml);
			out.add("}\n");
			return;
		}

		var mtForeach = extractAttribute(xml, MT_FOREACH);
		if (mtForeach != null){
			var o = extractExpressionTarget(mtForeach);
			if (o.target == null)
				throw "repeat/foreach requires two parameters (expression was '"+mtForeach+"')";
			out.add("var loop = "+parseExpression(o.exp)+";\n");
			out.add("__ctx.vars.repeat_"+o.target+" = new_repeat(loop);\n");
			out.add("iter(loop, function(__item){\n");
			out.add("__ctx.vars.repeat_"+o.target+".next(__item);\n");
			out.setVar(o.target, "__item");
			parseNode(xml);
			out.add("});\n");
			return;
		}

		var mtReplace = extractAttribute(xml, MT_REPLACE);
		if (mtReplace != null){
			echoExpression(mtReplace);
			return;
		}

		var mtOmitTag = extractAttribute(xml, MT_OMIT_TAG);
		if (mtOmitTag == null && xml.nodeName == MT){
			mtOmitTag = "true";
		}

		var mtAttributes = extractAttribute(xml, MT_ATTRIBUTES);
		var mtContent = extractAttribute(xml, MT_CONTENT);

		var hasContent = (mtContent != null || xml.firstChild() != null);

		var xhtmlEmpty = isXHTMLEmptyTag(xml.nodeName);
		if (xhtmlMode && hasContent && xhtmlEmpty){
			hasContent = false;
		}
		if (xhtmlMode && !hasContent && !xhtmlEmpty){
			hasContent = true;
		}

		if (mtOmitTag == null){
			out.writeHtml("<"+xml.nodeName);
			if (mtAttributes != null){
				doMtAttributes(mtAttributes, xml);
			}
			else {
				echoAttributes(xml);
			}
			if (hasContent){
				out.writeHtml(">");
			}
			else {
				out.writeHtml("/>");
				return;
			}
		}

		if (mtContent != null){
			echoExpression(mtContent);
		}
		else {
			for (child in xml)
				parseNode(child);
		}

		if (mtOmitTag == null && hasContent){
			out.writeHtml("</" + xml.nodeName + ">");
		}
	}

	function doMtAttributes( att:String, xml:Xml ){
		var overwritten = new Hash();
		var parts = Lambda.array(splitExpression(att));
		for (i in 0...parts.length){
			var x = StringTools.trim(parts[i]);
			var o = extractExpressionTarget(x);
			var exp = parseExpression(o.exp);
			if (isBooleanAttribute(o.target)){
				out.add("if ("+exp+"){\n");
				out.add("__out.add(\" "+o.target+"=\\\""+o.target+"\\\"\");\n");
				out.add("}");
			}
			else {
				out.add("var value = "+exp+";\n");
				out.add("if (value != false && value != null){\n");
				out.add("__out.add(\" "+o.target+"=\\\"\");\n");
				out.add("__out.add(html_escape(value));\n");
				out.add("__out.add(\"\\\"\");\n");
				out.add("}");
			}
			overwritten.set(o.target, true);
		}

		for (field in xml.attributes()){
			var attName = field;
			if (!overwritten.exists(attName)){
				var attVal = xml.get(field);
				if (attVal != null){ // mt attributes
					out.add("__out.add(\" "+attName+"=\\\"\");\n");
					echoString(attVal);
					out.add("__out.add(\"\\\"\");\n");
				}
			}
		}
	}

	function splitExpression( exp:String ) : List<String> {
		var result = new List();
		var start = 0;
		var len = exp.length;
		var i = 0;
		var inString = false;
		var inDString = false;
		while( i < len ){
			var c = exp.charAt(i);
			if (inString || inDString){
				if ( c == "\\" && ( (inString && exp.charAt(i+1) == "'") || (inDString && exp.charAt(i+1) == "\"") ) )
					i = i + 1;
				else if (inString && c == "'")
					inString = false;
				else if (inDString && c == "\"")
					inDString = false;
			}
			else if (c == "'")
				inString = true;
			else if (c == "\"")
				inDString = true;
			else if (c == ";"){
				result.push(exp.substr(start, i-start));
				start = i+1;
			}
			i++;
		}
		if (start < len){
			result.push(exp.substr(start, len-start));
		}
		return result;
	}

	function echoAttributes( xml:Xml ){
		if (xml == null)
			return;
		for (att in xml.attributes()){
			var value = xml.get(att);
			if (value == null)
				continue;
			out.writeHtml(" "+att);
			out.writeHtml("=\"");
			echoString( StringTools.replace(value, "\"", "&quot;") );
			out.writeHtml("\"");
		}
	}

	function parseComment( xml:Xml ){
		out.writeHtml("<!--"+StringTools.htmlUnescape(xml.nodeValue)+"-->");
	}

	function echoExpression( exp:String ){
		exp = StringTools.trim(exp);
		if (exp.indexOf("raw ", 0) == 0){
			out.writeCode(parseExpression(exp.substr(4, exp.length-4)));
		}
		else {
			out.writeEscapedCode(parseExpression(exp));
		}
	}

	function echoString( str:String ){
		var source = str;
		while (REGEX_DXP.match(source)){
			var pos = REGEX_DXP.matchedPos();
			if (pos.pos > 0){
				out.writeHtml(source.substr(0,pos.pos));
			}
			echoExpression(StringTools.htmlUnescape(REGEX_DXP.matched(1)));
			source = source.substr(pos.pos + pos.len, source.length - pos.pos - pos.len);
		}
		if (source.length > 0){
			out.writeHtml(source);
		}
	}

	function parseCDATA( xml:Xml ){
		out.writeHtml("<![CDATA[\n");
		var cdataSrc = xml.nodeValue;
		cdataSrc = StringTools.htmlUnescape(cdataSrc);
		cdataSrc = "<mt>" + cdataSrc + "</mt>";
		var cdataxml = null;
		try {
			cdataxml = Xml.parse(cdataSrc);
			if (cdataxml == null)
				throw "Unable to parse CDATA content";
		}
		catch (e:Dynamic){
			throw { error:e, xmlsource:cdataSrc, cdatasource:xml.nodeValue };
		}
		restoreCDATAHtmlEncoding(cdataxml);
		parseNode(cdataxml);
		out.writeHtml("]]>");
	}

	function isBooleanAttribute( attName:String ) : Bool {
		if (!xhtmlMode)
			return false;
		for (f in XHTML_ATTRS){
			if (f == attName) return true;
		}
		return false;
	}

	function isXHTMLEmptyTag( tag:String ) : Bool {
		if (!xhtmlMode)
			return false;
		for (f in XHTML_EMPTY){
			if (f == tag) return true;
		}
		return false;
	}

	static function splitArguments( str:String ) : List<String> {
		var res = new List();
		var arg = "";
		var len = str.length;
		var cto = 0;
		var string = false;
		var dstring = false;
		var i = 0;
		while (i < len){
			var c = str.charAt(i);
			if (c == "("){
				cto++;
			}
			else if (c == ")"){
				cto--;
			}

			if (c == "\\"){
				arg += c;
				arg += str.charAt(i+1);
				i += 2;
				continue;
			}

			if (c == "\"" && !string)
				dstring = !dstring;

			if (c == "'" && !dstring)
				string = !string;

			if (c == "," && cto == 0 && !string && !dstring){
				res.add(StringTools.trim(arg));
				arg = "";
			}
			else {
				arg += c;
			}
			i++;
		}
		if (arg != ""){
			res.add(StringTools.trim(arg));
		}
		return res;
	}

	static function findEndOfArray( str:String, pos:Int ) : { end:Int, n:Int } {
		var len = str.length;
		var n = 0;
		var ctopen = 0;
		for (i in (pos+1)...(len)){
			var c = str.charAt(i);
			if (c == "," && ctopen == 0){
				n++;
			}
			if (c == "["){
				ctopen++;
			}
			else if (c == "]"){
				if (ctopen == 0){
					if (StringTools.trim(str.substr(pos+1, i-pos-1)) != "")
						n++;
					return { end:i-1, n:n };
				}
				else {
					ctopen--;
				}
			}
		}
		return { end:-1, n:null };
	}

	static function findEndOfBracket( str:String, pos:Int ) : Int {
		var len = str.length;
		var ctopen = 0;
		for (i in (pos+1)...(len)){
			var c = str.charAt(i);
			if (c == "("){
				ctopen++;
			}
			else if (c == ")"){
				if (ctopen == 0){
					return i-1;
				}
				else {
					ctopen--;
				}
			}
		}
		return -1;
	}

	static function extractAttribute( xml:Xml, id:String ) : String {
		var res = xml.get(id);
		if (res == null){
			return null;
		}
		xml.set(id, null);
		return StringTools.trim(res.split("&quot;").join("\"").split("&amp;").join("&"));
	}

	static function restoreCDATAHtmlEncoding( xml:Xml ){
		if (xml.nodeType == Xml.Element || xml.nodeType == Xml.Document)
			for (x in xml) restoreCDATAHtmlEncoding(x);
		else
			xml.nodeValue = StringTools.htmlUnescape(xml.nodeValue);
	}

	static function extractExpressionTarget( exp:String ) : { target:String, exp:String } {
		if (REGEX_EXP.match(exp)){
			return {target:REGEX_EXP.matched(1), exp:REGEX_EXP.matched(2)};
		}
		return {target:null, exp:exp};
	}

	static function isExpressionKeyword( varName:String ) : Bool {
		return varName == "true" || varName == "false" || varName == "null" || varName == "if" || varName == "else";
	}

	// Quick and 'dirty' expression transformer,
    // This function transform a template expression into a neko compliant expression.
	public static function parseExpression( exp:String ) : String {
		var r_num = ~/[0-9]+/;
		var r_digit = ~/[0-9.]+/;
		var r_var = ~/[\$a-zA-Z0-9_]/;
		var r_op = ~/[!+-\/*<>=&|%]+/;  //*/
		var result = new StringBuf();
		var states = { none:0, string:1, dstring:2, variable:3, num:4, member:5, array:6 };
		var str = StringTools.trim(exp);
		var state = states.none;
		var mark = 0;
		var getter = false;
		var len = str.length;
		var i = 0;
		while (i < len+1){
			var skip = false;
			var c = if (i == len) "\n" else str.charAt(i);
			var n = if (i+1 >= len) "\n" else str.charAt(i+1);
			switch (state){
				case states.none:
					if (r_num.match(c)){
						state = states.num;
					}
					else if (c == "\""){
						result.add("String.new(");
						state = states.dstring;
					}
					else if (c == "'"){
						result.add("String.new(\"");
						state = states.string;
						skip = true;
					}
					else if (c == "."){
						state = states.member;
						if (i < len && str.charAt(i+1) == "_"){
							result.add(".get");
							getter = true;
							skip = true;
						}
					}
					else if (r_op.match(c)){
						if (c == '!' && n != "="){
							result.add("false == ");
							skip = true;
						}
						else if (c == "&" && n != "&" && n != "=" && len-i >= 5){
							if (str.substr(i,5) == "&amp;"){
								result.add("&"); i+=4; skip = true;
							}
							if (str.substr(i,4) == "&lt;"){
								result.add("<"); i+=3; skip = true;
							}
							else if (str.substr(i,4) == "&gt;"){
								result.add(">"); i+=3; skip = true;
							}
						}
					}
					else if (c == "("){
						var end = findEndOfBracket(str, i);
						if (end == -1)
							throw "Missing end ) in expression '"+exp+"'";
						var sub = str.substr(i+1, end-i);
						result.add("(");
						result.add(parseExpression(sub));
						i = end;
						skip = true;
					}
					else if (c == "["){
						var def = findEndOfArray(str, i);
						if (def.end == -1)
							throw "Missing end ] in expression '"+exp+"'";
						var sub = str.substr(i+1, def.end-i);
						result.add("Array.new1($array(");
						result.add(parseExpression(sub));
						result.add("), ");
						result.add(def.n);
						i = def.end;
						skip = true;
					}
					else if (c == "]"){
						result.add(")");
						skip = true;
					}
					else if (r_var.match(c)){
						state = states.variable;
						mark = i;
					}

				case states.string:
					if (c == "\\" && n == "'"){
						result.add("'");
						skip = true;
						++i;
					}
					else if (c == "\\" && n == "\""){
						skip = true;
					}
					else if (c == "'"){
						state = states.none;
						result.add("\")");
						skip = true;
					}

				case states.dstring:
					if (c == "\\" && n == "\""){
						result.add("\\\"");
						skip = true;
						++i;
					}
					else if (c == "\\" && n == "'"){
						skip = true;
					}
					else if (c == "\""){
						state = states.none;
						result.add("\")");
						skip = true;
					}

				case states.variable:
					if (r_var.match(c)){
					}
					else if (c == "."){
						var variable = str.substr(mark, i-mark);
						if (variable == "repeat"){
							result.add("__ctx.vars.repeat_");
							state = states.member;
							skip = true;
						}
						else {
							result.add("__ctx.get(");
							result.add(Generator.hash(variable));
							result.add(")");
							state = states.member;
							if (i < len && str.charAt(i+1) == "_"){
								result.add(".get");
								getter = true;
								skip = true;
							}
						}
					}
					else {
						var variable = str.substr(mark, i-mark);
						if (isExpressionKeyword(variable)){
							result.add(variable);
						}
						else if (c == "["){
							result.add("__ctx.get(");
							result.add(Generator.hash(variable));
							result.add(")");
							var def = findEndOfArray(str, i);
							if (def.end == -1)
								throw "Missing end ] in expression '"+exp+"'";
							var sub = str.substr(i+1, def.end-i);
							result.add("[");
							result.add(parseExpression(sub));
							result.add("]");
							i = def.end + 1;
							skip = true;
							state = states.none;
						}
						else {
							result.add("__ctx.get(");
							result.add(Generator.hash(variable));
							result.add(")");
						}
						state = states.none;
					}

				case states.num:
					if (!r_digit.match(c)){
						state = states.none;
					}

				case states.member:
					if (r_var.match(c)){
					}
					else if (c == "("){
						if (getter){
							result.add("()");
							getter = false;
						}
						var end = findEndOfBracket(str, i);
						if (end == -1)
							throw "Missing end bracket in expression '"+exp+"'";
						var sub = str.substr(i+1, end-i);
						var argStr = Lambda.array(splitArguments(sub));
						for (j in 0...argStr.length){
							argStr[j] = parseExpression(argStr[j]);
						}
						result.add("(");
						result.add(argStr.join(","));
						result.add(")");
						i = end+1; skip = true;
					}
					else if (c == "."){
						if (getter){
							result.add("()");
							getter = false;
						}
						if (i < len && str.charAt(i+1) == "_"){
							getter = true;
							result.add(".get");
							skip = true;
						}
					}
					else if (c == "["){
						if (getter){
							result.add("()");
							getter = false;
						}
						var def = findEndOfArray(str, i);
						if (def.end == -1)
							throw "Missing end ] in expression '"+exp+"'";
						var sub = str.substr(i+1, def.end-i);
						result.add("[");
						result.add(parseExpression(sub));
						result.add("]");
						i = def.end + 1;
						skip = true;
						state = states.none;
					}
					else {
						if (getter){
							result.add("()");
							getter = false;
						}
						state = states.none;
						if (i != len){
							i--;
						}
						skip = true;
					}
			}
			if (!skip && i < len && state != states.variable){
				result.add(str.charAt(i));
			}
			++i;
		}
		return result.toString();
	}
}
