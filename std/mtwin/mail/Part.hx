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

import neko.Utf8;

class Part extends MetaPart<Part> {
	public static function parseString( str : String ) : Part {
		var o = new Part();
		o.parse( str );
		return o;
	}

	public function new(?a,?b,?c){
		super(a,b,c);
	}

	public override function newPart( ctype : String ) : Part {
		var o = new Part( ctype, true, charset );
		this.addPart( o );
		return o;
	}
}

class MetaPart<T> {
	static var headerOrder = [
		"Return-Path","Received","Date","From","Subject","Sender","To",
		"Cc","Bcc","Content-Type","X-Mailer","X-Originating-IP","X-Originating-User"
	];

	static var REG_HEADER = ~/^([a-zA-Z0-9_\-]+):(.*)$/;
	static var REG_CRLF_END = ~/(\r?\n)$/;
	
	//////////////////
	
	public var content : String;
	public var parts : List<T>;
	var headers : Hash<Array<String>>;
	var contentType : String;
	var charset : String;
	var boundary : String;
	public var name : String;
	var id : String;
	var subPart : Bool;

	public function new( ?ctype : String, ?sp : Bool, ?charset : String ){
		this.contentType = if( ctype == null ) "text/plain" else ctype;
		this.subPart = sp == true;
		this.charset = if( charset == null ) "iso-8859-1" else charset;

		content = "";
		
		parts = new List();
		headers = new Hash();
	}

	function getContentType(){
		return contentType;
	}

	public function addPart( part : T ){
		parts.push( part );
	}

	public function setContent( c : String ){
		setHeader( "Content-Transfer-Encoding", "quoted-printable" );
		content = c;
	}

	public function setContentFromFile(filename:String,type:String){
		var a = filename.split("/");
		name = a.pop();
		content = neko.io.File.getContent(filename);
		contentType = type;
		setHeader("Content-Type",type+"; name=\""+name+"\"");
		setHeader("Content-Disposition","attachment; filename=\""+name+"\"");
		setHeader("Content-Transfer-Encoding","base64");
	}
	
	public function setHeader( name : String, content : String ){
		if( headers.exists(name) ){
			var l = headers.get(name);
			if( l.length > 1 )
				throw "Unable to setHeader, multiple header.";

			l[0] = content;
		}else{
			headers.set(name,[content]);
		}
	}

	public function getHeader( name: String, ?cs : String ){
		if( !headers.exists(name) ){
			return null;
		}
		var r = headers.get(name)[0];

		if( cs != null && cs != charset ){
			var cslc = cs.toLowerCase();
			var charsetlc = charset.toLowerCase();
			if( cslc != "utf-8" && charsetlc == "utf-8" ){
				r =  Utf8.decode( r );
			}else if( charsetlc != "utf-8" && cslc == "utf-8" ){
				r =  Utf8.encode( r );
			}
		}

		return r;
	}

	public function addHeader( name : String, content : String ){
		if( headers.exists(name) ){
			headers.get(name).push(content);
		}else{
			headers.set(name,[content]);
		}		
	}

	public function setDate( ?d : Date ){
		if( d == null ) d = Date.now();
		setHeader("Date",DateTools.format(d,"%a, %e %b %Y %H:%M:%S %z"));
	}

	public function setContentId( ?cid : String ) : String {
		if( cid == null ){
			var t = getHeader("Content-Id");
			if( t != null ){
				return t.substr(1,t.length-2);
			}

			cid = Tools.randomEight()+"."+Tools.randomEight();

			setHeader("Content-Id","<"+cid+">");
		}else{
			setHeader("Content-Id","<"+cid+">");
		}

		return cid;
	}

	public function htmlUseContentId( p : Hash<String> ){
		for( filename in p.keys() ){
			content = StringTools.replace( content, filename, "cid:"+p.get(filename) );
		}
	}

	static var REG_START_TAB = ~/^(\t| )+/;
	function htmlRemoveTab(){
		content = REG_START_TAB.replace(content,"");
	}

	public function get() : String {
		var boundary = "";

		if( parts.length > 0 ){
			if( contentType.substr(0,10).toLowerCase() != "multipart/" ){
				contentType = "multipart/mixed";
			}

			if( boundary == null || boundary.length == 0 ){
				boundary = "----=" + Tools.randomEight() + "_" + Tools.randomEight() + "." + Tools.randomEight();
			}

			setHeader("Content-Type",contentType+"; charset=\""+charset+"\";\r\n\tboundary=\""+boundary+"\"");
		}else{
			setHeader("Content-Type",contentType+"; charset=\""+charset+"\"");
		}

		if( !subPart ){
			setHeader("MIME-Version","1.0");
			setHeader("X-Mailer","haXe mailer");
		}

		var ret = new StringBuf();
		
		// copy headers
		var myHeaders = new Hash();
		for( k in headers.keys() ){
			myHeaders.set(k,headers.get(k));
		}
		
		// Put standard headers
		for( p in headerOrder ){
			if( myHeaders.exists(p) ){
				for( s in myHeaders.get(p) )
					ret.add(Tools.formatHeader(p,s,charset));
				myHeaders.remove(p);
			}
		}
		
		// Put other headers
		for( k in myHeaders.keys() ){
			for( s in myHeaders.get(k) )
				ret.add(Tools.formatHeader(k,s,charset));
		}

		ret.add("\r\n");

		// Add content
		if( content.length > 0 ){
			switch( getHeader("Content-Transfer-Encoding") ){
				case "base64":
					ret.add( Tools.encodeBase64(content) + "\r\n" );
				case "quoted-printable":
					ret.add( Tools.encodeQuotedPrintable(content) + "\r\n" );
				default:
					ret.add( content + "\r\n" );
			}
		}
		
		// Add parts
		if( parts.length > 0 ){
			var pcp = new List<MetaPart<T>>();
			for( p in parts ){
				pcp.add(cast p);
			}
			
			if( contentType == "multipart/alternative" ){
				// text/plain first
				for( v in pcp ){
					if( v.contentType == "text/plain" ){
						ret.add( "--" + boundary + "\r\n" + v.get() );
						pcp.remove(v);
					}
				}
				
				// then text/html
				for( v in pcp ){
					if( v.contentType == "text/html" ){
						ret.add( "--" + boundary + "\r\n" + v.get() );
						pcp.remove(v);
					}
				}
			}

			for( v in pcp ){
				ret.add( "--" + boundary + "\r\n" + v.get() );
			}
			ret.add("--" + boundary + "--\r\n");
			
		}

		return ret.toString();
	}


	function parse( str:String, ?id : String ){
		if( str == null )
			throw "unable to parse null";
			
		if( id == null ) subPart = false;
		this.id = id;

		var arr = Tools.splitLines(str);
		var head : List<String> = new List();

		var inHead = true;
		var buf = new StringBuf();
		
		for( ln in arr ){
			if( !inHead ){
				buf.add( Tools.removeCRLF(ln) );
				buf.add("\r\n");
			}else{
				if( StringTools.trim(ln).length == 0 ){
					inHead = false;
					head.add( buf.toString() );
					buf = new StringBuf();
				}else{
					var nbTab = Tools.countInitTab(ln);
					if( nbTab > 0 ){
						buf.add(" ");
						buf.add( Tools.removeCRLF(ln.substr(nbTab,ln.length-nbTab)) );
					}else{
						head.add( buf.toString() );
						buf = new StringBuf();
						buf.add( Tools.removeCRLF(ln) );
					}
				}
			}
		}
		content = buf.toString();
		
		for( ln in head ){
			if( REG_HEADER.match(ln) ){
				var name = Tools.formatHeaderTitle(REG_HEADER.matched(1));
				var value = StringTools.trim(REG_HEADER.matched(2));
				if( headers.exists(name) ){
					headers.get(name).push( value );
				}else{
					headers.set(name,[value]);
				}				
			}
		}

		var ctype0 = "text";
		var ctype1 = "plain";

		// parse contentType
		var hctype = Tools.parseComplexHeader(getHeader("Content-Type"));
		if( hctype != null ){
			var t = hctype.value.split("/");
			ctype0 = StringTools.trim(t[0]).toLowerCase();
			ctype1 = StringTools.trim(t[1]).toLowerCase();

			if( hctype.params.exists("charset") ){
				charset = hctype.params.get("charset");
			}

			if( hctype.params.exists("boundary") ){
				boundary = hctype.params.get("boundary");
			}

			if( hctype.params.exists("name") ){
				name = hctype.params.get("name");
			}
		}
		
		contentType = ctype0+"/"+ctype1;

		for( k in headers.keys() ){
			var a = headers.get(k);
			for( i in 0...a.length ){
				a[i] = Tools.headerDecode(a[i],charset);
			}
		}

		if( ctype0 == "multipart" ){
			if( boundary == null || boundary.length == 0 ){
				contentType = "text/plain";
				ctype0 = "text";
				ctype1 = "plain";
			}else{
				splitContent();
			}
		}

		if( headers.exists("Content-Transfer-Encoding") ){
			var cte = getHeader("Content-Transfer-Encoding").toLowerCase();
			if( cte == "quoted-printable" ){
				content = Tools.decodeQuotedPrintable( content );
			}else if( cte == "base64" ){
				content = Tools.decodeBase64( content );
			}
		}

		var cdispo = Tools.parseComplexHeader(getHeader("Content-Disposition"));
		if( cdispo != null && cdispo.params.exists("filename") ){
			name = cdispo.params.get("filename");
		}
	}

	function splitContent(){
		var bound = Tools.pregQuote(boundary);
		var regStr = "(.*?)--"+bound+"(.*)--"+bound+"--";
		var reg = new EReg(regStr,"s");

		if( reg.match( content ) ){
			content = reg.matched(1);

			var tmp = reg.matched(2).split("--"+boundary);

			var myId = if(id == null || id.length == 0) "" else id + ".";
			var i = 0;
			for( str in tmp ){
				i++;
				if( REG_CRLF_END.match(str) ){
					str = str.substr(0,-REG_CRLF_END.matched(1).length);
				}
				
				var p = cast newPart("text/plain");
				p.parse( StringTools.trim(str), myId+i );
			}
		}
	}

	public function newPart(ctype:String) : T {
		throw "Part cannot be used directly : newPart need to be overrided";
		return null;
	}
}
