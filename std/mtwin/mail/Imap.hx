package mtwin.mail;

import neko.Socket;
import mtwin.mail.Exception;

signature ImapConnectionInfo {
	host: String, 
	port: Int, 
	user: String, 
	pass: String
}

signature ImapMailbox {
	name: String,
	flags: List<String>,
	hasChildren: Bool
}


class Imap {
	public static var DEBUG = false;

	var cnx : Socket;
	var count : Int;

	static var REG_CRLF = ~/\r?\n/g;
	function rmCRLF(s){
		return REG_CRLF.replace(s, "");
	}

	function debug(s:String){
		if( DEBUG ) neko.Lib.print(Std.string(s)+"\n");
	}

	function quote( s : String ) : String {
		return "\""+s.split("\"").join("\\\"")+"\"";
	}
	
	public function new(args: ImapConnectionInfo){
		count = 0;
		cnx = new Socket();
		connect( args.host, args.port );
		login( args.user, args.pass );
	}

	function command( command, args, r ){
		count++;
		var c = Std.string(count);
		c = StringTools.lpad(c,"A000",4);
		cnx.write( c+" "+command+" "+args+"\r\n" );
		debug( "S: "+c+" "+command+" "+args );

		if( !r ){
			return null;
		}
		return read(c);
	}

	static var REG_RESP = ~/(OK|NO|BAD) (\[([^\]]+)\] )?(([A-Z]{2,}) )? ?(.*)/;
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

	function connect( host : String, port : Int ){
		try{
			cnx.connect( Socket.resolve(host), port );
		}catch( e : Dynamic ){
			throw new ConnectionError(host,port);
		}
		debug("socket connected");
		cnx.setTimeout( 1 );
		cnx.readLine();
	}

	function login( user : String, pass : String ){	
		var r = command("LOGIN",user+" "+pass,true);
		if( !r.success ){
			throw new BadResponse(r.response);
		}
	}
	
	/////////
	//
	static var REG_EXISTS = ~/^([0-9]+) EXISTS$/;
	static var REG_RECENT = ~/^([0-9]+) RECENT$/;
	static var REG_UNSEEN = ~/^OK \[UNSEEN ([0-9]+)\]/;
	
	public function select( mailbox : String ){
		var r = command("SELECT",quote(mailbox),true);
		if( !r.success ) 
			throw new BadResponse(r.response);
		
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

	static var REG_LIST_RESP = ~/LIST \(([ \\A-Za-z0-9]*)\) "\." "([^"]+)"/;
	public function mailboxes( ?pattern : String ) : List<ImapMailbox> {
		var r;
		if( pattern == null ){
			r = command("LIST","\".\" \"*\"",true);
		}else{
			r = command("LIST","\".\" \""+pattern+"\"",true);
		}
		if( !r.success ){
			throw new BadResponse(r.response);
		}

		var ret = new List();
		for( v in r.result ){
			if( REG_LIST_RESP.match(v) ){	
				var name = REG_LIST_RESP.matched(2);
				var flags = REG_LIST_RESP.matched(1).split(" ");

				var t = {name: name,flags: new List(),hasChildren: false};

				for( v in flags ){
					if( v == "" ) continue;
					
					if( v == "\\HasNoChildren" ){
						t.hasChildren = false;
					}else if( v == "\\HasChildren" ){
						t.hasChildren = true;
					}

					t.flags.add( v );
				}

				ret.add(t);
			}
		}
		return ret;
	}

	public function search( ?pattern : String ){
		if( pattern == null ) pattern = "ALL";
		var r = command("SEARCH",pattern,true);
		if( !r.success ){
			throw new BadResponse(r.response);
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

	public function fetchSearch( pattern : String, ?section : String ){
		if( section == null ) section = "BODY.PEEK[]";
		var r = search(pattern);
		if( r.length == 0 ) return new IntHash();

		return fetchRange( r.join(","), section );
	}

	public function fetchOne( id : Int, ?section : String, ?useUid : Bool ) {
		if( section == null ) section = "BODY.PEEK[]";
		if( useUid == null ) useUid = false;

		var r = fetchRange( Std.string(id), section, useUid );
		if( !r.exists(id) ){
			throw new ImapFetchError(id);
		}
		return r.get(id);
	}

	static var REG_FETCH_MAIN = ~/([0-9]+) FETCH \(/;
	static var REG_FETCH_PART = ~/^(BODY\[[A-Za-z0-9.]*\]|RFC822\.?[A-Z]*) \{([0-9]+)\}/;
	static var REG_FETCH_FLAGS = ~/^FLAGS \(([ \\A-Za-z0-9$]*)\) */;
	static var REG_FETCH_UID = ~/^UID ([0-9]+) */;
	static var REG_FETCH_BODYSTRUCTURE = ~/^BODY(STRUCTURE)? \(/;
	static var REG_FETCH_END = ~/^([A0-9]{4}) (OK|BAD|NO)/;
	public function fetchRange( range : String, ?section : String, ?useUid : Bool ){
		if( range == null ) return null;
		if( section == null ) section = "BODY[]";
		if( useUid == null ) useUid = false;
		
		if( useUid )
			command("UID FETCH",range+" "+section,false);
		else
			command("FETCH",range+" "+section,false);

		var ret = new IntHash();
		while( true ){
			var l = cnx.readLine();
			if( REG_FETCH_MAIN.match(l) ){
				var id = Std.parseInt(REG_FETCH_MAIN.matched(1));
				
				var o = if( ret.exists(id) ){
					ret.get(id); 
				}else {
					var o = {type: null,content: null,flags: null,uid: null,structure: null};
					ret.set(id,o);
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
					}else if( REG_FETCH_BODYSTRUCTURE.match( s ) ){
						var t = REG_FETCH_BODYSTRUCTURE.matchedRight().substr(0,-1);
						o.structure = ImapBodyStructure.parse( t );
						break;
					}else if( REG_FETCH_PART.match( s ) ){
						var type = REG_FETCH_PART.matched(1);
						var len = Std.parseInt(REG_FETCH_PART.matched(2));
						
						o.content = cnx.read( len );
						o.type = type;
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
					throw new BadResponse(l);
				}
			}else{
				throw new UnknowResponse(l);
			}
		}
		
		if( useUid ){
			var old = ret;
			ret = new IntHash();
			for( e in old ){
				ret.set(e.uid,e);
			}
		}

		return ret;
	}
}
