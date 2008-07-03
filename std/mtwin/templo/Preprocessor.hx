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

class Preprocessor {

	static var r_if      = ~/::if([^_a-zA-Z0-9].*?)::/gs;
	static var r_elseif  = ~/::elseif([^_a-zA-Z0-9].*?)::/gs;
	static var r_else    = ~/::else\s*?::/;
	static var r_foreach = ~/::foreach\s+(.*?)::/gs;
	static var r_fill    = ~/::fill\s+(.*?)::/gs;
	static var r_use     = ~/::use\s+(.*?)::/gs;
	static var r_set     = ~/::set\s+(.*?)::/gs;

	static var r_cond    = ~/::cond\s+(.*?)::/gs;
	static var r_repeat  = ~/::repeat\s+(.*?)::/gs;
	static var r_attr    = ~/::attr\s+(.*?)::/gs;

	static var r_cdata     = ~/<!\[CDATA\[([^\0]*?)]]>/g;
	static var r_comment   = ~/<!--([^\0]*?)-->/g;
	static var r_macroCall = ~/\$\$([a-zA-Z0-9_]+)\(/g;
	static var r_print     = ~/::(.*?)::/g;

	public static var macros : Hash<mtwin.templo.Macro> = new Hash();
	public static var macroFileStamp : Float;

	public static function process( str:String ) : String {
		if (macroFileStamp == null && Loader.MACROS != null)
			registerMacroFile(Loader.BASE_DIR+Loader.MACROS);

		var res = str;
		res = escapeCdata1(res);
		res = expandMacros(res);
		res = res.split("::else::").join("</mt><mt mt:else=\"\">");
		res = res.split("::end::").join("</mt>");
		while (r_if.match(res)){
			res = res.split(r_if.matched(0)).join("<mt mt:if=\""+quote(StringTools.trim(r_if.matched(1)))+"\">");
		}
		while (r_elseif.match(res)){
			res = res.split(r_elseif.matched(0)).join("</mt><mt mt:elseif=\""+quote(StringTools.trim(r_elseif.matched(1)))+"\">");
		}
		while (r_foreach.match(res)){
			res = res.split(r_foreach.matched(0)).join("<mt mt:foreach=\""+quote(r_foreach.matched(1))+"\">");
		}
		while (r_fill.match(res)){
			res = res.split(r_fill.matched(0)).join("<mt mt:fill=\""+quote(r_fill.matched(1))+"\">");
		}
		while (r_set.match(res)){
			res = res.split(r_set.matched(0)).join("<mt mt:set=\""+quote(r_set.matched(1))+"\"/>");
		}
		while (r_use.match(res)){
			res = res.split(r_use.matched(0)).join("<mt mt:use=\""+quote(r_use.matched(1))+"\">");
		}
		res = escapeComments(res);

		while (r_cond.match(res)){
			res = res.split(r_cond.matched(0)).join("mt:if=\""+quote(r_cond.matched(1))+"\"");
		}
		while (r_repeat.match(res)){
			res = res.split(r_repeat.matched(0)).join("mt:foreach=\""+quote(r_repeat.matched(1))+"\"");
		}
		while (r_attr.match(res)){
			res = res.split(r_attr.matched(0)).join("mt:attr=\""+quote(r_attr.matched(1))+"\"");
		}
		res = unescapeCdata1(res);
		res = escapePrints(res);
		return trimExtraSpaces("<mt>" + res + "</mt>");
	}

	static function trimExtraSpaces( str:String ) : String {
		var reg = null;

		reg = ~/^\s+(<mt mt:[a-z]+="[^"]+"\/?>)\s*?\n/gm;
		while (reg.match(str)) str = StringTools.replace(str, reg.matched(0), reg.matched(1));

		reg = ~/^\s+(<\/?mt>)\s*?\n/gm;
		while (reg.match(str)) str = StringTools.replace(str, reg.matched(0), reg.matched(1));

		reg = ~/^\s+(&lt;mt mt:[a-z]+="[^"]+"\/?&gt;)\s*?\n/gm;
		while (reg.match(str)) str = StringTools.replace(str, reg.matched(0), reg.matched(1));

		reg = ~/^\s+(&lt;\/?mt&gt;)\s*?\n/gm;
		while (reg.match(str)) str = StringTools.replace(str, reg.matched(0), reg.matched(1));

		reg = ~/\s+((<|&lt;)mt mt:(set)="[^"]+"\/?(>|&gt;))\s+/g;
		while (reg.match(str)) str = StringTools.replace(str, reg.matched(0), reg.matched(1));

		reg = ~/(<mt mt:[^=]+="[^"]+"\/?>)\s+(<\/?mt)/g;
		while (reg.match(str)) str = StringTools.replace(str, reg.matched(0), reg.matched(1)+reg.matched(2));

		reg = ~/(&lt;mt mt:[^=]+="[^"]+"\/?&gt;)\s+(&lt;\/?mt)/g;
		while (reg.match(str)) str = StringTools.replace(str, reg.matched(0), reg.matched(1)+reg.matched(2));
		
		reg = ~/(&lt;\/mt&gt;)\s+(&lt;\/?mt)/g;
		while (reg.match(str)) str = StringTools.replace(str, reg.matched(0), reg.matched(1)+reg.matched(2));

		return str;
	}

	public static function expandMacros( str:String ) : String {
		var res = str;
		while (r_macroCall.match(res)){
			var macroName = r_macroCall.matched(1);
			var macroCallPos = r_macroCall.matchedPos();
			var i = macroCallPos.pos + macroCallPos.len;
			var args = new Array();
			var nargs = 0;
			var startArg = i;
			var endArg = -1;
			var oAccos = 0;
			var oParas = 0;
			var end = 0;
			var forceArg = false;
			while (i < res.length){
				var c = res.charAt(i);
				if (c == "("){
					if (oAccos == 0){
						oParas++;
						++i;
						continue;
					}
				}
				if (c == ")" && oParas > 0){
					oParas--;
					++i;
					continue;
				}
				if (c == "{"){
					if (oAccos == 0){ 
						startArg = i+1;
						forceArg = true; 
					}
					oAccos++;
				}
				if (c == "}"){
					if (oAccos > 0){ 
						oAccos--;
						if (oAccos == 0){ endArg = i; }
					}
				}
				if (oAccos == 0 && oParas == 0 && (c == "," || c == ")")){
					if (endArg == -1){ endArg = i; }
					var p = res.substr(startArg, endArg - startArg);
					p = StringTools.trim(p);
					if (p.length > 0 || forceArg){
						args[ nargs ] = p;
						nargs++;
					}
					startArg = i+1;
					endArg = -1;
					forceArg = false;
				}
				if (oAccos == 0 && (c == ")")){
					end = i+1;
					break;
				}
				++i;
			}
			var mcr = macros.get(macroName);
			if (mcr == null){
				throw "Unknown macro "+macroName;
			}
			var src = res.substr(r_macroCall.matchedPos().pos, end - r_macroCall.matchedPos().pos);
			res = res.split(src).join(mcr.expand(args));
		}
		return res;
	}

	public static function registerMacroFile( path:String ){
		if (!neko.FileSystem.exists(path)){
			throw "Macro file "+path+" does not exists";
		}
		macroFileStamp = neko.FileSystem.stat(path).mtime.getTime();
		registerMacros(neko.io.File.getContent(path));
	}

	public static function registerMacros( src:String ){
		src = StringTools.replace(src, "\r\n", "\n");
		src = StringTools.replace(src, "\r", "\n");
		var rFind = ~/<macro\s+name=['"](.*?)['"]\s*?>([^\0]*?)<\/macro>/g; //"
		while (rFind.match(src)){
			var pos = rFind.matchedPos();
			var name = rFind.matched(1);
			var content = rFind.matched(2);
			var macro = new mtwin.templo.Macro(name, content);
			mtwin.templo.Preprocessor.macros.set( macro.name, macro );
			var end = pos.pos + pos.len;
			src = src.substr(end, src.length - end);
		}
	}

	static function quote( str:String ) : String {
		return str.split("&").join("&amp;").split("\"").join("&quot;");
	}

	static function escapeCdata1( str:String ) : String {
		var res = str;
		while (r_cdata.match(res)){
			var pos   = r_cdata.matchedPos();
			var cdata = r_cdata.matched(1);
			cdata = cdata.split("&").join("&amp;").split("<").join("&lt;").split(">").join("&gt;");
			res = res.substr(0, pos.pos) + "<![CDATAX[" + cdata + "]]>" + res.substr(pos.pos+pos.len, res.length-(pos.pos+pos.len));
		}
		return res;
	}

	static function escapeComments( str:String ) : String {
		var res = str;
		while (r_comment.match(res)){
			var pos = r_comment.matchedPos();
			var comment = r_comment.matched(1);
			comment = comment.split("&").join("&amp;").split("<").join("&lt;").split(">").join("&gt;");
			res = res.substr(0, pos.pos) + "<![COMMENT[" + comment + "]]C>" + res.substr(pos.pos+pos.len, res.length-(pos.pos+pos.len));
		}
		return res;
	}

	static function unescapeCdata1( str:String ) : String {
		var res = str.split("<![CDATAX[").join("<![CDATA["); //.split("]]>").join("]]>");
		res = escapeCdata1(res);
		res = res.split("<![CDATAX[").join("<![CDATA["); // .split("]]>").join("]]>");
		res = res.split("<![COMMENT[").join("<!--").split("]]C>").join("-->");
		return res;
	}

	static function escapePrints( str:String ) : String {
		var buf = new StringBuf();
		while (r_print.match(str)){
			var pos = r_print.matchedPos();
			buf.add(str.substr(0,pos.pos));
			buf.add(StringTools.htmlEscape(r_print.matched(0)));
			str = str.substr(pos.pos+pos.len, str.length);
		}
		buf.add(str);
		return buf.toString();
	}
}
