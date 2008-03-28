package mtwin.mail;

class Pop {
	var host : String;
	var user : String;
	var pass : String;
	var port : Int;

	var sock : neko.net.Socket;

	public function new( host : String, user : String, pass : String, ?port : Int ){
		this.host = host;
		this.user = user;
		this.pass = pass;
		this.port = port == null ? 110 : port;
	}
	
	private function connect(){
		if( sock == null ){
			var s = new neko.net.Socket();
			s.setTimeout(1);
			s.connect(new neko.net.Host(host),port);
			
			// get server init line
			s.input.readLine();

			s.output.write("USER "+user+"\n");
			var ret = s.input.readLine();
			if( ret.substr(0,3) != "+OK" ) throw "Pop USER failed: "+ret;

			s.output.write("PASS "+pass+"\n");
			var ret = s.input.readLine();
			if( ret.substr(0,3) != "+OK" ) throw "Pop PASS failed: "+ret;

			sock = s;
		}
		return sock;
	}

	public function close(){
		if( sock != null ){
			sock.output.write("QUIT\n");
			var ret = sock.input.readLine();
			if( ret.substr(0,3) != "+OK" ) throw "Pop QUIT failed: "+ret;
			sock.close();
			sock = null;
		}
	}

	static var REG_SCAN_LIST = ~/([0-9]+) ([0-9]+)/;
	public function list() : List<{id: Int,size: Int}> {
		var cnx = connect();

		cnx.output.write("LIST\n");
		var ret = new List();
		var t : String;
		do {
			t = StringTools.trim(cnx.input.readLine());
			if( REG_SCAN_LIST.match(t) )
				ret.add({
					id: Std.parseInt(REG_SCAN_LIST.matched(1)),
					size: Std.parseInt(REG_SCAN_LIST.matched(2))
				});
		}while( t != null && t != ".");
		return ret;
	}

	public function getMessage( id : Int ){
		var cnx = connect();

		cnx.output.write("RETR "+id+"\n");
		var r = cnx.input.readLine();
		if( r.substr(0,3) != "+OK" ) throw "Pop RETR failed: "+r;

		var ret = new StringBuf();
		do {
			var l = StringTools.rtrim(cnx.input.readLine());
			if( l == "." )
				break;
			if( l.substr(0,2) == ".." )
				l = l.substr(1);
			ret.add(l);
			ret.add("\r\n");
		}while( true );
		return ret.toString().substr(0,-2);
	}

	public function delete( id : Int ){
		var cnx = connect();

		cnx.output.write("DELE "+id+"\n");
		var r = cnx.input.readLine();
		if( r.substr(0,3) != "+OK" ) throw "Pop DELE failed: "+r;
	}
}
