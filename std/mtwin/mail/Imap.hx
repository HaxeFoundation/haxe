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

import neko.Socket;
import mtwin.mail.Exception;

signature ImapMailbox = {
	name: String,
	flags: ImapFlags,
	hasChildren: Bool
}

signature ImapFetchResponse = {
	id: Int,
	uid: Int,
	bodyType: String,
	body: String,
	flags: ImapFlags,
	structure: ImapBodyStructure,
	internalDate: String,
	envelope: ImapEnvelope
}

signature ImapFlags = Array<String>

enum ImapSection {
	Flags;
	Uid;
	BodyStructure;
	Envelope;
	InternalDate;
	Body(ss:ImapBodySection);
	BodyPeek(ss:ImapBodySection);
}

enum ImapBodySection {
	Header;
	Mime;
	Text;
	SubSection(id:String,ss:ImapBodySection);
}

enum ImapRange {
	Single(i:Int);
	Range(s:Int,e:Int);
	Composite(l:Array<ImapRange>);
}

enum ImapFlagMode {
	Add;
	Remove;
	Replace;
}

class Imap {
	public static var DEBUG = false;
	public static var TIMEOUT = 5;

	var cnx : Socket;
	var count : Int;

	static var REG_RESP = ~/(OK|NO|BAD) (\[([^\]]+)\] )?(([A-Z]{2,}) )? ?(.*)/;
	static var REG_EXISTS = ~/^([0-9]+) EXISTS$/;
	static var REG_RECENT = ~/^([0-9]+) RECENT$/;
	static var REG_UNSEEN = ~/^OK \[UNSEEN ([0-9]+)\]/;
	static var REG_FETCH_MAIN = ~/([0-9]+) FETCH \(/;
	static var REG_FETCH_PART = ~/^(BODY\[[A-Za-z0-9.]*\]|RFC822\.?[A-Z]*) \{([0-9]+)\}/;
	static var REG_FETCH_FLAGS = ~/^FLAGS \(([ \\A-Za-z0-9$]*)\) */;
	static var REG_FETCH_UID = ~/^UID ([0-9]+) */;
	static var REG_FETCH_BODYSTRUCTURE = ~/^BODY(STRUCTURE)? \(/;
	static var REG_FETCH_ENVELOPE = ~/^ENVELOPE \(/;
	static var REG_FETCH_INTERNALDATE = ~/^INTERNALDATE "([^"]+)" */;
	static var REG_FETCH_END = ~/^([A0-9]{4}) (OK|BAD|NO)/;
	static var REG_LIST_RESP = ~/LIST \(([ \\A-Za-z0-9]*)\) "\." "([^"]+)"/;
	static var REG_CRLF = ~/\r?\n/g;

	static function rmCRLF(s){
		return REG_CRLF.replace(s, "");
	}

	static function quote( s : String ) : String {
		return "\""+s.split("\"").join("\\\"")+"\"";
	}

	static function debug(s:String){
		if( DEBUG ) neko.Lib.print(Std.string(s)+"\n");
	}

	//////

	public function new(){
		count = 0;
	}

	/**
		Connect to Imap Server
	**/
	public function connect( host : String, ?port : Int ){
		if( cnx != null ) throw AlreadyConnected;

		if( port == null ) port = 143;
		cnx = new Socket();
		try{
			cnx.connect( Socket.resolve(host), port );
		}catch( e : Dynamic ){
			throw ConnectionError(host,port);
		}
		debug("socket connected");
		cnx.setTimeout( TIMEOUT );
		cnx.readLine();
	}

	/**
		Login to server
	**/
	public function login( user : String, pass : String ){	
		var r = command("LOGIN",user+" "+pass);
		if( !r.success ){
			throw BadResponse(r.response);
		}
	}

	/**
		Close connection to server
	**/
	public function close(){
		cnx.close();
		cnx = null;
	}

	/**
		List mailboxes that match pattern (all mailboxes if pattern is null)
	**/
	public function mailboxes( ?pattern : String ) : List<ImapMailbox> {
		var r;
		if( pattern == null ){
			r = command("LIST","\".\" \"*\"");
		}else{
			r = command("LIST","\".\" \""+pattern+"\"");
		}
		if( !r.success ){
			throw BadResponse(r.response);
		}

		var ret = new List();
		for( v in r.result ){
			if( REG_LIST_RESP.match(v) ){	
				var name = REG_LIST_RESP.matched(2);
				var flags = REG_LIST_RESP.matched(1).split(" ");

				var t = {name: name,flags: flags,hasChildren: false};

				for( v in flags ){
					if( v == "\\HasNoChildren" ){
						t.hasChildren = false;
					}else if( v == "\\HasChildren" ){
						t.hasChildren = true;
					}
				}

				ret.add(t);
			}
		}
		return ret;
	}

	/**
		Select a mailbox
	**/
	public function select( mailbox : String ){
		var r = command("SELECT",quote(mailbox));
		if( !r.success ) 
			throw BadResponse(r.response);
		
		var ret = {recent: 0,exists: 0,firstUnseen: null};
		for( v in r.result ){
			if( REG_EXISTS.match(v) ){
				ret.exists = Std.parseInt(REG_EXISTS.matched(1));
			}else if( REG_UNSEEN.match(v) ){
				ret.firstUnseen = Std.parseInt(REG_UNSEEN.matched(1));
			}else if( REG_RECENT.match(v) ){
				ret.recent = Std.parseInt(REG_RECENT.matched(1));
			}
		}

		return ret;
	}

	/**
		Search for messages. Pattern syntax described in RFC 3501, section 6.4.4
	**/
	public function search( ?pattern : String, ?useUid : Bool ){
		if( pattern == null ) pattern = "ALL";
		if( useUid == null ) useUid = false;

		var r = command(if( useUid) "UID SEARCH" else "SEARCH",pattern);
		if( !r.success ){
			throw BadResponse(r.response);
		}

		var l = new List();

		for( v in r.result ){
			if( StringTools.startsWith(v,"SEARCH ") ){
				var t = v.substr(7,v.length-7).split(" ");
				for( i in t ){
					l.add( Std.parseInt(i) );
				}
			}
		}

		return l;
	}

	/**
		Search for messages, fetch those found.
	**/
	public function fetchSearch( pattern : String, ?section : Array<ImapSection> ) : List<ImapFetchResponse>{
		if( section == null ) section = [BodyPeek(null)];
		var r = search(pattern);
		if( r.length == 0 ) return new List();

		var t = new Array<ImapRange>();
		for( i in r ){
			t.push(Single(i));
		}

		return fetchRange( Composite(t), section );
	}

	/**
		Fetch one message by its id ou uid
	**/
	public function fetchOne( id : Int, ?section : Array<ImapSection>, ?useUid : Bool ) {
		if( section == null ) section = [BodyPeek(null)];
		if( useUid == null ) useUid = false;

		var r = fetchRange( Single(id), section, useUid );
		if( r.length != 1 ){
			throw ImapFetchError(id);
		}
		return r.first();
	}

	/**
		Fetch messages from the currently selected mailbox.
	**/
	public function fetchRange( iRange: ImapRange, ?iSection : Array<ImapSection>, ?useUid : Bool ) : List<ImapFetchResponse>{
		if( iRange == null ) return null;
		if( iSection == null ) iSection = [Body(null)];
		if( useUid == null ) useUid = false;

		var range = Tools.imapRangeString(iRange);
		var section = Tools.imapSectionString(iSection);

		if( useUid )
			command("UID FETCH",range+" "+section,false);
		else
			command("FETCH",range+" "+section,false);

		var tmp = new IntHash();
		var ret = new List();
		while( true ){
			var l = cnx.readLine();
			if( REG_FETCH_MAIN.match(l) ){
				var id = Std.parseInt(REG_FETCH_MAIN.matched(1));
				
				var o = if( tmp.exists(id) ){
					tmp.get(id); 
				}else {
					var o = {bodyType: null,body: null,flags: null,uid: null,structure: null,internalDate: null,envelope: null,id: id};
					tmp.set(id,o);
					ret.add(o);
					o;
				}

				var s = REG_FETCH_MAIN.matchedRight();
				while( s.length > 0 ){
					if( REG_FETCH_FLAGS.match( s ) ){
						o.flags = REG_FETCH_FLAGS.matched(1).split(" ");
						s = REG_FETCH_FLAGS.matchedRight();
					}else if( REG_FETCH_UID.match( s ) ){
						o.uid = Std.parseInt(REG_FETCH_UID.matched(1));
						s = REG_FETCH_UID.matchedRight();
					}else if( REG_FETCH_INTERNALDATE.match( s ) ){
						o.internalDate = REG_FETCH_INTERNALDATE.matched(1);
						s = REG_FETCH_INTERNALDATE.matchedRight();
					}else if( REG_FETCH_ENVELOPE.match( s ) ){
						var t = REG_FETCH_ENVELOPE.matchedRight();
						t = completeString(t);
						o.envelope = ImapEnvelope.parse( t );
						s = StringTools.ltrim(t.substr(o.envelope.__length,t.length));
					}else if( REG_FETCH_BODYSTRUCTURE.match( s ) ){
						var t = REG_FETCH_BODYSTRUCTURE.matchedRight();
						t = completeString(t);
						o.structure = ImapBodyStructure.parse( t );
						s = StringTools.ltrim(t.substr(o.structure.__length,t.length));
					}else if( REG_FETCH_PART.match( s ) ){
						var len = Std.parseInt(REG_FETCH_PART.matched(2));
						
						o.body = cnx.read( len );
						o.bodyType = REG_FETCH_PART.matched(1);
						
						cnx.readLine();
						break;
					}else{
						break;
					}
				}
				
			}else if( REG_FETCH_END.match(l) ){
				var resp = REG_FETCH_END.matched(2);
				if( resp == "OK" ){
					break;
				}else{
					throw BadResponse(l);
				}
			}else{
				throw UnknowResponse(l);
			}
		}
		
		return ret;
	}

	/**
		Append content as a new message at the end of mailbox.
	**/
	public function append( mailbox : String, content : String, ?flags : ImapFlags ){
		var f = if( flags != null ) "("+flags.join(" ")+") " else "";
		command("APPEND",quote(mailbox)+" "+f+"{"+content.length+"}",false);
		cnx.write( content );
		cnx.write( "\r\n" );
		var r = read( StringTools.lpad(Std.string(count),"A000",4) );
		if( !r.success )
			throw BadResponse(r.response);
	}
	
	/**
		Remove permanently all messages flagged as \Deleted in the currently selected mailbox.
	**/
	public function expunge(){
		var r = command("EXPUNGE");
		if( !r.success )
			throw BadResponse(r.response);
	}

	/**
		Add, remove or replace flags on message(s) of the currently selected mailbox.
	**/
	public function flags( iRange : ImapRange, flags : ImapFlags, ?mode : ImapFlagMode, ?useUid : Bool, ?fetchResult : Bool ) : IntHash<Array<String>> {
		if( mode == null ) mode = Add;
		if( fetchResult == null ) fetchResult = false;
		if( useUid == null ) useUid = false;
		
		var range = Tools.imapRangeString(iRange);
		var elem = switch( mode ){
			case Add: "+FLAGS";
			case Remove: "-FLAGS";
			case Replace: "FLAGS";
		}
		if( !fetchResult ){
			elem += ".SILENT";
		}

		var r = command( if( useUid ) "UID STORE" else "STORE", range + " " + elem + "("+flags.join(" ")+")");
		if( !r.success ) throw BadResponse( r.response );
		if( !fetchResult ) return null;

		var ret = new IntHash();
		for( line in r.result ){
			if( REG_FETCH_MAIN.match(line) ){
				var id = Std.parseInt(REG_FETCH_MAIN.matched(1));
				if( REG_FETCH_FLAGS.match( REG_FETCH_MAIN.matchedRight() ) ){
					ret.set(id,REG_FETCH_FLAGS.matched(1).split(" "));
				}
			}
		}
		return ret;
	}

	/**
		Create a new mailbox.
	**/
	public function create( mailbox : String ){
		var r = command( "CREATE", quote(mailbox) );
		if( !r.success ) throw BadResponse( r.response );
	}
	
	/**
		Delete a mailbox.
	**/
	public function delete( mailbox : String ){
		var r = command( "DELETE", quote(mailbox) );
		if( !r.success ) throw BadResponse( r.response );
	}

	/**
		Rename a mailbox.
	**/
	public function rename( mailbox : String, newName : String ){
		var r = command( "RENAME", quote(mailbox)+" "+quote(newName) );
		if( !r.success ) throw BadResponse( r.response );
	}

	/**
		Copy message(s) from the currently selected mailbox to the end of an other mailbox.
	**/
	public function copy( iRange : ImapRange, toMailbox : String, ?useUid : Bool ){
		if( useUid == null ) useUid = false;

		var range = Tools.imapRangeString(iRange);
		var r = command(if(useUid) "UID COPY" else "COPY",range+" "+quote(toMailbox));
		if( !r.success ) throw BadResponse( r.response );
	}

	public function sort( criteria : String, ?pattern : String, ?charset : String, ?useUid : Bool ){
		if( pattern == null ) pattern = "ALL";
		if( useUid == null ) useUid = false;
		if( charset == null ) charset = "US-ASCII";

		var r = command(if( useUid) "UID SORT" else "SORT","("+criteria+") "+charset+" "+pattern);
		if( !r.success ){
			throw BadResponse(r.response);
		}

		var l = new List();

		for( v in r.result ){
			if( StringTools.startsWith(v,"SORT ") ){
				var t = v.substr(5,v.length-5).split(" ");
				for( i in t ){
					l.add( Std.parseInt(i) );
				}
			}
		}

		return l;
	}

	public function fetchSort( criteria : String, ?pattern : String ,?section : Array<ImapSection> ) : List<ImapFetchResponse>{
		if( section == null ) section = [BodyPeek(null)];
		if( pattern == null ) pattern = "ALL";
		var r = sort(criteria);
		if( r.length == 0 ) return new List();

		var t = new Array<ImapRange>();
		for( i in r ){
			t.push(Single(i));
		}

		return fetchRange( Composite(t), section );
	}


	/////

	function completeString( s ){
		var reg = ~/(?<!\] )\{([0-9]+)\}$/;
		while( reg.match( s ) ){
			var len = Std.parseInt( reg.matched(1) );
			var t = cnx.read( len );
			var e = cnx.readLine();
			s = s.substr(0,-reg.matchedPos().len)+"\""+t.split("\"").join("\\\"")+"\"" +e;
		}
		return s;
	}

	function command( command : String, ?args : String , ?r : Bool ){
		if( cnx == null )
			throw NotConnected;
		if( r == null ) r = true;
		if( args == null ) args = "" else args = " "+args;
		
		count++;
		var c = Std.string(count);
		c = StringTools.lpad(c,"A000",4);
		cnx.write( c+" "+command+args+"\r\n" );
		debug( "S: "+c+" "+command+args );

		if( !r ){
			return null;
		}
		return read(c);
	}

	
	function read( c ){
		var resp = new List();
		var sb : StringBuf = null;
		while( true ){
			var line = cnx.readLine();
			debug("R: "+line);
			line = rmCRLF(line);

			if( c != null && line.substr(0,4) == c ){
				if( REG_RESP.match(line.substr(5,line.length-5)) ){
					if( sb != null ){
						resp.add( sb.toString() );
					}
					return {
						result: resp,
						success: REG_RESP.matched(1) == "OK",
						error: REG_RESP.matched(1),
						command: REG_RESP.matched(4),
						response: REG_RESP.matched(6),
						comment: REG_RESP.matched(3)
					};
				}else{
					throw UnknowResponse(line);
				}
			}else{
				if( StringTools.startsWith(line,"* ") ){
					if( sb != null ){
						resp.add( sb.toString() );
					}
					sb = new StringBuf();
					sb.add( line.substr(2,line.length - 2) );
				}else{
					if( sb != null ){
						sb.add( line+"\r\n" );
					}else{
						resp.add( line );
					}
				}
			}
		}
		return null;
	}	
}
