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
package mtwin.mail;

typedef Address = {
	name: String,
	address: String
}

class Tools {

	static var BASE64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
	static var HEXA = "0123456789ABCDEF";


	static var REG_HEADER_DECODE = ~/^(.*?)=\?([^\?]+)\?(Q|B)\?([^?]*)\?=\s?(.*?)$/i;
	static var REG_QP_LB = ~/=\\r?\\n/;
	static var REG_QP = ~/=([A-Fa-f0-9]{1,2})/;
	static var REG_START_TAB = ~/^(\t| )+/;
	
	public static function chunkSplit( str:String, length:Int, sep:String ){
		var ret = "";
		while( str.length > length ){
			ret += str.substr(0,length) + sep;
			str = str.substr(length,str.length - length);
		}
		return ret + str;
	}

	public static function splitLines( str : String ) : Array<String> {
		var ret = str.split("\n");
		for( i in 0...ret.length ){
			var l = ret[i];
			if( l.substr(-1,1) == "\r" ){
				ret[i] = l.substr(0,-1);
			}
		}
		return ret;
	}

	public static function encodeBase64( content : String ){
		return StringTools.rtrim(chunkSplit(StringTools.baseEncode( content, BASE64 ), 76, "\r\n")) + "==";
	}

	public static function decodeBase64( content : String ){
		return StringTools.baseDecode( StringTools.replace(StringTools.replace(content,"\r\n",""),"=",""), BASE64 );
	}

	public static function encodeQuotedPrintable( content : String ) : String{
		var rs = new List();
		var lines = splitLines( content );
		
		for( ln in lines ){
			var len = ln.length;
			var line = "";
			for( i in 0...len ){
				var c = ln.charAt(i);
				var o = c.charCodeAt(0);
				if( o == 9 ){
				}else if( o < 16 ){
					c = "=0" + StringTools.baseEncode(c,HEXA);
				}else if( o == 61 || o < 32 || o > 126 ){
					c = "=" + StringTools.baseEncode(c,HEXA);
				}

				// space at the end of line
				if( i == len - 1 ){
					if( o == 32 ){
						c = "=20";
					}else if( o == 9 ){
						c = "=09";
					}
				}

				// soft line breaks
				var ll = line.length;
				var cl = c.length;
				if( ll + cl >= 76 && (i != len -1 || ll + cl != 76) ){
					rs.add(line + "=");
					line = "";
				}
				line += c;
			}
			rs.add(line);
		}

		return rs.join("\r\n");
	}

	public static function decodeQuotedPrintable( str : String ){
		str = ~/=\r?\n/g.replace(str,"");
		var a = str.split("=");
		var first = true;
		var ret = new StringBuf();
		for( t in a ){
			if( first ){
				first = false;
				ret.add(t);
			}else{
				ret.add(StringTools.baseDecode(t.substr(0,2).toUpperCase(),HEXA) + t.substr(2,t.length - 2));
			}			
		}
		return ret.toString();
	}

	// TODO Protect address in "non ascii chars" <add@re.ss>
	public static function headerQpEncode( ostr : String, initSize : Int, charset : String ){
		var str = ~/\r?\n\s*/.replace(ostr," ");
		
		var csl = charset.length;
		var len = str.length;
		var quotedStr : List<String> = new List();
		var line = new StringBuf();
		var llen = 0;
		var useQuoted = false;
		for( i in 0...len ){
			var c = str.charAt(i);
			var o = c.charCodeAt(0);

			if( o == 9 ){
			}else if( o < 16 ){
				useQuoted = true;
				c = "=0" + StringTools.baseEncode(c,HEXA);
			}else if( o == 61 || o == 58 || o == 63 || o == 95 || o == 34 ){
				c = "=" + StringTools.baseEncode(c,HEXA);
			}else if( o < 32 || o > 126 ){
				useQuoted = true;
				c = "=" + StringTools.baseEncode(c,HEXA);
			}else if( o == 32 ){
				c = "_";
			}

			// max line length = 76 - 17 ( =?iso-8859-1?Q?...?= ) => 59 - initSize
			var max : Int;
			if( quotedStr.length == 0 ){
				max = 69 - csl - initSize;
			}else{
				max = 69 - csl;
			}
			var clen = c.length;
			if( llen + clen >= max ){
				quotedStr.add(line.toString());
				line = new StringBuf();
				llen = 0;
			}
			line.add(c);
			llen += clen;
		}
		quotedStr.add(line.toString());

		if( !useQuoted ){
			return wordWrap(ostr,75,"\r\n\t",initSize);
		}else{
			return "=?"+charset+"?Q?"+quotedStr.join("?=\r\n\t=?"+charset+"?Q?")+"?=";
		}
	}

	public static function headerDecode( str : String, charsetOut : String ){
		str = ~/\r?\n\s*/.replace(str," ");
		while( REG_HEADER_DECODE.match(str) ){
			var charset = StringTools.trim(REG_HEADER_DECODE.matched(2).toLowerCase());
			var encoding = StringTools.trim(REG_HEADER_DECODE.matched(3).toLowerCase());
			var encoded = StringTools.trim(REG_HEADER_DECODE.matched(4));

			var start = REG_HEADER_DECODE.matched(1);
			var end = REG_HEADER_DECODE.matched(5);

			if( encoding == "q" ){
				encoded = decodeQuotedPrintable(StringTools.replace(encoded,"_"," "));
			}else if( encoding == "b" ){
				encoded = decodeBase64(encoded);
			}else{
				throw "Unknow transfer-encoding: "+encoding;
			}

			charsetOut = charsetOut.toLowerCase();
			if( charsetOut != "utf-8" && charset == "utf-8" ){
				encoded =  neko.Utf8.decode( encoded );
			}else if( charset != "utf-8" && charsetOut == "utf-8" ){
				encoded =  neko.Utf8.encode( encoded );
			}

			str = start + encoded + end;
		}

		return str;
	}

	public static function removeCRLF( str ){
		return StringTools.replace(StringTools.replace(str,"\n",""),"\r","");
	}

	public static function formatHeaderTitle( str : String ) : String {
		str = StringTools.trim( str );
		if( str.toLowerCase() == "mime-version" ) return "MIME-Version";

		var arr = str.split("-");
		for( i in 0...arr.length ){
			var t = arr[i];
			arr[i] = t.substr(0,1).toUpperCase()+t.substr(1,t.length-1).toLowerCase();
		}
		return arr.join("-");
	}

	public static function countInitTab( str : String ) : Int {
		if( REG_START_TAB.match(str) ){
			return REG_START_TAB.matched(0).length;
		}else{
			return 0;
		}
	}

	public static function randomEight(){
		var s = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

		var ret = "";
		for( i in 0...8 ){
			ret += s.charAt(Std.random(s.length));
		}
		return ret;
	}

	public static function pregQuote( str : String ){
		str = StringTools.replace(str,"\\","\\\\");
		str = StringTools.replace(str,".","\\.");
		str = StringTools.replace(str,"+","\\+");
		str = StringTools.replace(str,"*","\\*");
		str = StringTools.replace(str,"?","\\?");
		str = StringTools.replace(str,"^","\\^");
		str = StringTools.replace(str,")","\\)");
		str = StringTools.replace(str,"(","\\(");
		str = StringTools.replace(str,"[","\\[");
		str = StringTools.replace(str,"]","\\]");
		str = StringTools.replace(str,"{","\\{");
		str = StringTools.replace(str,"}","\\}");
		str = StringTools.replace(str,"=","\\=");
		str = StringTools.replace(str,"!","\\!");
		str = StringTools.replace(str,"<","\\<");
		str = StringTools.replace(str,">","\\>");
		str = StringTools.replace(str,"|","\\|");
		str = StringTools.replace(str,":","\\:");
		str = StringTools.replace(str,"$","\\$");
		str = StringTools.replace(str,"/","\\/");
			
		return str;
	}

	public static function formatHeader( name : String, content : String, charset : String ){
		// TODO if To, From, Cc, Bcc, use a special encode to keep addresses
		return name+": "+headerQpEncode(content,name.length,charset)+"\r\n";
	}

	static var REG_MHEADER = ~/^([^;]+)(.*?)$/;
	static var REG_PARAM1 = ~/^; *([a-zA-Z]+)="(([^"]|\\")+)"/;
	static var REG_PARAM2 = ~/^; *([a-zA-Z]+)=([^;]+)/;
	public static function parseComplexHeader( h : String ){
		if( h == null ) return null;

		var ret = {value: null, params: new Hash()};
		if( REG_MHEADER.match(h) ){
			ret.value = StringTools.trim( REG_MHEADER.matched(1) );

			var params = REG_MHEADER.matched(2);
			while( params.length > 0 ){
				params = StringTools.ltrim( params );

				if( REG_PARAM1.match( params ) ){
					var k = StringTools.trim(REG_PARAM1.matched(1)).toLowerCase();
					var v = REG_PARAM1.matched(2);
					ret.params.set( k, v ); 
					params = REG_PARAM1.matchedRight();
				}else if( REG_PARAM2.match( params ) ){
					var k = StringTools.trim(REG_PARAM2.matched(1)).toLowerCase();
					var v = StringTools.trim(REG_PARAM2.matched(2));
					ret.params.set( k, v );
					params = REG_PARAM2.matchedRight();
				}else{
					break;
				}
			}
		}else{
			ret.value = h;
		}
		return ret;

	}

	public static function wordWrap( str : String, ?length : Int, ?sep : String, ?initCur : Int ){
		if( length == null ) length = 75;
		if( sep == null ) sep = "\n";
		if( initCur == null ) initCur = 0;
		
		var reg = ~/(.*?)(\s+)/;
		var a = new Array();
		while( reg.match(str) ){
			var c = reg.matched(1);
			var s = reg.matched(2);
			var l = c.length+s.length;
			a.push( c );
			a.push( s );
			str = str.substr(l,str.length-l);
		}
		a.push( str );

		var sb = new StringBuf();
		var cur = initCur;
		for( i in 0...a.length ){
			var e = a[i];
			var l = e.length;
			var cut = false;
			if( cur+l > length ){
				sb.add( sep );
				cur = 0;
				cut = true;
			}
			if( i % 2 != 1 || !cut ){
				sb.add( e );
				cur += l;
			}
		}

		return sb.toString();
	}

	// TODO routes & groups ?
	static var REG_ADDRESS = ~/^((([^()<>@,;:\\"\[\]\s[:cntrl:]]+)|"((\"|[^"])*)")+@[A-Z0-9][A-Z0-9-.]*)/i;
	static var REG_ROUTE_ADDR = ~/^<((([^()<>@,;:\\"\[\]\s[:cntrl:]]+)|"((\"|[^"])*)")+@[A-Z0-9][A-Z0-9-.]*)>/i;
	static var REG_ATOM = ~/^([^()<>@,;:\\".\[\]\s[:cntrl:]]+)/i;
	static var REG_QSTRING = ~/^"((\"|[^"])*)"/;
	static var REG_SEPARATOR = ~/,\s*/;
	public static function parseAddress( str : String ) : Array<Address> {
		var a = new Array();
		var name = null;
		var address = null;

		str = StringTools.trim(str);
		var s = str;

		while( s.length > 0 ){
			s = StringTools.ltrim(s);
			if( REG_ADDRESS.match(s) ){
				if( address != null ) throw Exception.ParseError(str+", near: "+s.substr(0,15));
				address = REG_ADDRESS.matched(1);
				s = REG_ADDRESS.matchedRight();
			}else if( REG_ROUTE_ADDR.match(s) ){
				if( address != null ) throw Exception.ParseError(str+", near: "+s.substr(0,15));
				address = REG_ROUTE_ADDR.matched(1);
				s = REG_ROUTE_ADDR.matchedRight();
			}else if( REG_ATOM.match(s) ){
				if( name != null ) name += " ";
				else name = "";
				name += REG_ATOM.matched(1);
				s = REG_ATOM.matchedRight();
			}else if( REG_QSTRING.match(s) ){
				var t = REG_QSTRING.matched(1);
				t = ~/\\(.)/g.replace(t,"$1");
				if( name != null ) name += " ";
				else name = "";
				name += t;
				s = REG_QSTRING.matchedRight();
			}else if( REG_SEPARATOR.match(s) ){
				if( address != null ){
					a.push({name: if( name != null && name.length > 0 ) name else null, address: address});
					address = null;
					name = null;
				}
				s = REG_SEPARATOR.matchedRight();
			}else{
				throw Exception.ParseError(str+", near: "+s.substr(0,15));
			}
		}

		if( address != null ){
			a.push({name: if( name != null && name.length > 0 ) name else null, address: address});
		}
		return a;
	}

	public static function formatAddress( a : Array<Address> ){
		var r = new List();
		for( c in a ){
			if( c.name == null || c.name == "" ) r.add(c.address);
			else if( ~/^[A-Z0-9 ]*$/i.match(c.name) ) r.add(c.name+" <"+c.address+">");
			else{
				var quoted = c.name.split("\\").join("\\\\").split("\"").join("\\\"");
				r.add("\""+quoted+"\" <"+c.address+">");
			}
		}
		return r.join(",");
	}

}
