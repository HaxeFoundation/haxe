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
package mtwin.mail.imap;

import mtwin.mail.Exception;

typedef Address = {
	name: String,
	address: String
}

class Envelope {
	public var __length : Int;
	public var date(default,null) : String;
	public var subject(default,null) : String;
	public var from(default,null) : List<Address>;
	public var sender(default,null) : List<Address>;
	public var replyTo(default,null) : List<Address>;
	public var to(default,null) : List<Address>;
	public var cc(default,null) : List<Address>;
	public var bcc(default,null) : List<Address>;
	public var inReplyTo(default,null) : String;
	public var messageId(default,null) : String;

	function new(){
	}
		
	public function getDate(){
		if( date == null )
			return null;
		return mtwin.DateFormat.parse(date);
	}

	public static function parse( s : String ){
		var len = s.length;
		var parCount = 0;
		var p = 0;
		var argPos = 0;
		var ret = new Envelope();
		var tmp = {
			alist: new List<Address>(),
			buf: new Array<String>()
		};

		var closeParenthesis = function(){
			if( parCount == 1 ){
				tmp.alist.add({name: tmp.buf[0],address: tmp.buf[2]+"@"+tmp.buf[3]});
				tmp.buf = new Array();
			}else if( parCount == 0 ){
				switch( argPos ){
					case 2: ret.from = tmp.alist;
					case 3: ret.sender = tmp.alist;
					case 4: ret.replyTo = tmp.alist;
					case 5: ret.to = tmp.alist;
					case 6: ret.cc = tmp.alist;
					case 7: ret.bcc = tmp.alist;
				}
				tmp.alist = new List();
			}
		};
		
		var addElement = function( e : String ){
			if( parCount >= 2 ){
				tmp.buf.push( e );
			}else{
				if( e != null ){
					switch( argPos ){
						case 0: ret.date = e;
						case 1: ret.subject = e;
						case 8: ret.inReplyTo = e;
						case 9: ret.messageId = e;
					}
				}
			}
		};

		while( p < len ){
			var c = s.charAt(p);
			p++;
			switch( c ){
				case "(":
					parCount++;
				case ")":
					parCount--;
					if( parCount < 0 ){
						ret.__length = p;
						return ret;
					}
					closeParenthesis();
				case "\"":
					var b = new StringBuf();
					var escape = false;
					while( p < len ){
						var c2 = s.charAt(p);
						p++;
						if( c2 == "\"" && !escape )
							break;
						escape = (c2 == "\\" && !escape );
						if( !escape )
						b.add( c2 );
					}
					addElement( b.toString() );
				case " ":
					if( parCount == 0 )
						argPos++;
				default:
					var b = new StringBuf();
					p--;
					while( p < len ){
						var c2 = s.charAt(p);
						p++;
						if( c2 == ")" || c2 == " " ){
							p--;
							break;
						}
						b.add( c2 );
					}
					var bs = b.toString();
					if( bs == "NIL" )
						addElement( null );
					else
						throw ParseError(bs);
			}
		}
		ret.__length = p;
		return ret;
	}
}
