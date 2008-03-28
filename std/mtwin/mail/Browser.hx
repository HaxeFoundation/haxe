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

typedef MainPart = {
	ctype_primary: String,
	ctype_secondary: String,
	charset: String,
	content: String
}

import neko.Utf8;
import mtwin.mail.Part;

class Browser extends MetaPart<Browser> {

	public static function parseString( str : String ) : Browser {
		var o = new Browser();
		o.parse( str );
		return o;
	}

	//////////

	public function new(?ctype : String, ?sp : Bool, ?charset : String){
		if( ctype == null ) ctype = "text/plain";
		if( sp == null ) sp = false;
		if( charset == null ) charset = "iso-8859-15";
		super( ctype, sp, charset );
	}

	public override function newPart( ctype : String ) : Browser {
		var o = new Browser( ctype, true, charset );
		this.addPart( o );
		return o;
	} 

	public function getMainPartCharset( cs : String, ?preferHtml : Bool ){
		var r = getMainPart(preferHtml);
		if( cs != r.charset ){
			var cslc = cs.toLowerCase();
			var charsetlc = r.charset.toLowerCase();

			if( cslc != "utf-8" && charsetlc == "utf-8" ){
				r.content =  Utf8.decode( r.content );
			}else if( charsetlc != "utf-8" && cslc == "utf-8" ){
				r.content =  Utf8.encode( r.content );
			}
			r.charset = cs;
		}
		return r;
	}

	public function getMainPart( ?preferHtml : Bool, ?level : Int, ?priority : Int, ?cpriority : Int ) : MainPart {
		if( level == null ) level = 0;
		if( priority == null ) priority = 0;
		if( cpriority == null ) cpriority = 0;
		if( preferHtml == null ) preferHtml = true;

		var ctype = contentType.split("/");
		var ctype0 = ctype[0];
		var ctype1 = ctype[1];

		if( ctype0 != "multipart" || (level == 0 && parts.length == 0) ){
			if( level == 0 ) return mkBody();
			if( preferHtml ){
				if( ctype1 == "html" ) return mkBody();
				if( ctype1 == "plain" && cpriority > 0 ) return mkBody();
			}else{
				if( ctype1 == "plain" ) return mkBody();
				if( ctype1 == "html" && cpriority > 0 ) return mkBody();
			}
		}else{
			if( level == 0 ){
				// multipart !
				// si c'est au premier niveau, c'est une boucle principale, avec priorité qui augmente
				do {
					do {
						var r = null;
						for( part in parts ){
							r = part.getMainPart( preferHtml, level + 1, priority, cpriority );
							if( r != null ) break;
						}
						if( r != null ) return r;
						priority++;
					}while( priority <= 1 );
					cpriority++;
				}while( cpriority <= 1 );
			}else{
				// là c'est des boucles qui se déclanche si c'est ok
				if( ctype1 == "alternative" || priority > 0 ){
					var r = null;
					for( part in parts ){
						r = part.getMainPart( preferHtml, level + 1, priority, cpriority );
						if( r != null ) return r;
					}
				}
			}
		}
		return null;
	}

	public function listAttachment( ?level : Int, ?cs : String ){
		if( cs == null )
			cs = charset;
		cs = cs.toLowerCase();
		if( level == null ) level = 0;
		var l = listAttachmentObjects( level );
		var r = new List();
		for( v in l ){
			var name = v.name;
			if( cs != "utf-8" && v.charset.toLowerCase() == "utf-8" ){
				name =  Utf8.decode( name );
			}else if( v.charset.toLowerCase() != "utf-8" && cs == "utf-8" ){
				name =  Utf8.encode( name );
			}
			r.add({
				name: name, 
				id: v.id, 
				type: v.contentType
			});
		}
		return r;
	}

	function listAttachmentObjects( level : Int ) : List<Browser> {
		var ctype = contentType.split("/");
		var ctype0 = ctype[0];
		var ctype1 = ctype[1];

		var ret = new List();
		if( ctype0 != "multipart" ){
			if( level != 0 && headers.exists("Content-Disposition") ){
				ret.add( this );
			}
		}else if( ctype1 != "alternative" ){
			for( part in parts ){
				for( v in part.listAttachmentObjects( level + 1 ) ){
					ret.add( v );
				}
			}
		}
		return ret;
	}

	public function getAttachment( i : String ) : {name: String,ctype: String,content: String }{
		if( id == i ){
			return {
				name: name,
				ctype: contentType,
				content: content
			};
		}
		for( part in parts ){
			var t = part.getAttachment( i );
			if( t != null ) return t;
		}
		return null;
	}

	public function getAttachmentByCid( wid : String ) : {name: String,ctype: String,content: String }{
		var cid = getHeader("Content-Id");
		if( cid != null && StringTools.trim(cid) == "<"+wid+">" ){
			return {
				name: name,
				ctype: contentType,
				content: content
			};
		}
		for( part in parts ){
			var t = part.getAttachmentByCid( wid );
			if( t != null ) return t;
		}
		return null;
	}
	
	public function hasHeader( name ){
		name = Tools.formatHeaderTitle( name );
		return headers.exists( name );
	}

	public override function getHeader( name, ?charset ){
		return super.getHeader( Tools.formatHeaderTitle(name),charset );
	}

	public function getAddress( name, ?charset ){
		var e = getHeader(name,charset);
		return Tools.parseAddress(e,false);
	}

	function mkBody() : {ctype_primary: String,ctype_secondary: String,charset: String,content: String} {
		var ctype = contentType.split("/");

		return {
			ctype_primary: ctype[0],
			ctype_secondary: ctype[1],
			charset: charset,
			content: content
		};
	}

	public function toString( ?level : Int ) : String {
		if( level == null ) level = 0;

		var s = StringTools.lpad("","\t",level);
		var s2 = StringTools.lpad("","\t",level+1);
		var sb = new StringBuf();
		
		sb.add( s );
		sb.add("mail.Browser#");
		sb.add(id);
		sb.add("<");
		sb.add(contentType);
		sb.add(">");

		var sb2 = new StringBuf();

		if( hasHeader("From") ){
			sb2.add(s2);
			sb2.add("From: ");
			sb2.add(getHeader("From","utf-8"));
			sb2.add("\n");
		}

		if( hasHeader("To") ){
			sb2.add(s2);
			sb2.add("To: ");
			sb2.add(getHeader("To","utf-8"));
			sb2.add("\n");
		}

		if( hasHeader("Subject") ){
			sb2.add(s2);
			sb2.add("Subject: ");
			sb2.add(getHeader("Subject","utf-8"));
			sb2.add("\n");
		}

		if( hasHeader("Date") ){
			sb2.add(s2);
			sb2.add("Date: ");
			sb2.add(getHeader("Date","utf-8"));
			sb2.add("\n");
		}		

		if( name != null ){
			sb2.add(s2);
			sb2.add("Name: ");
			sb2.add(name);
			sb2.add("\n");
		}

		for( part in parts ){
			sb2.add( part.toString( level + 1 ) );
		}

		var t = sb2.toString();

		if( t.length > 0 ){
			sb.add(" [\n");
			sb.add(t);
			sb.add(s);
			sb.add("]");
		}
		
		sb.add("\n");
		return sb.toString();
	}

}
