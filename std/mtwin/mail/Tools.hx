package mtwin.mail;

class Tools {

	static var BASE64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
	static var HEXA = "0123456789ABCDEF";


	static var REG_HEADER_DECODE = ~/^(.*?)=\?([^\?]+)\?(Q|B)\?([^?]*)\?=(.*?)$/i;
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

	public static function encodeBase64( content : String , crlf : String ){
		return StringTools.rtrim(chunkSplit(StringTools.baseEncode( content, BASE64 ), 76, crlf)) + "==";
	}

	public static function decodeBase64( content : String, crlf ){
		return StringTools.baseDecode( StringTools.replace(StringTools.replace(content,crlf,""),"=",""), BASE64 );
	}

	public static function encodeQuotedPrintable( content : String, crlf : String ) : String{
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

		return rs.join(crlf);
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
		var str = removeCRLF(ostr);
		
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
			return ostr;
		}else{
			return "=?"+charset+"?Q?"+quotedStr.join("?=\r\n\t=?"+charset+"?Q?")+"?=";
		}
	}

	public static function headerDecode( str : String, charsetOut : String ){
		while( REG_HEADER_DECODE.match(str) ){
			var charset = StringTools.trim(REG_HEADER_DECODE.matched(2).toLowerCase());
			var encoding = StringTools.trim(REG_HEADER_DECODE.matched(3).toLowerCase());
			var encoded = StringTools.trim(REG_HEADER_DECODE.matched(4));

			var start = REG_HEADER_DECODE.matched(1);
			var end = REG_HEADER_DECODE.matched(5);

			if( encoding == "q" ){
				encoded = decodeQuotedPrintable(StringTools.replace(encoded,"_"," "));
			}else if( encoding == "b" ){
				encoded = decodeBase64(encoded,"\r\n");
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

}
