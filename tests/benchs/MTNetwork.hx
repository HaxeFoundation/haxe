class Const {

	public static var HOST = new neko.net.Host("localhost");
	public static var PORT = 1234;
	public static var LOOPS = 3000;
	public static var CLIENTS = 200;
	public static var SOCKETS = 5;
	public static var SERVERS = 16;
	public static var USE_POLL = true;

}

class Task  {

	var t : neko.vm.Thread;
	public var done : Bool;

	public function new() {
		t = neko.vm.Thread.create(run);
	}

	function run() {
		try {
			loop();
		} catch( e : Dynamic ) {
			neko.Lib.print(e);
			neko.Lib.print("\n");
			neko.Lib.print(haxe.Stack.toString(haxe.Stack.exceptionStack()));
			neko.Lib.print("\n");
		}
		done = true;
	}

	function loop() {
		// TODO
	}

}

class Client extends Task {

	public var time : Float;

	public function new() {
		time = 0;
		super();
	}

	function loop() {
		var sockets = new Array();
		for( i in 0...Const.SOCKETS ) {
			var s = new neko.net.Socket();
			while( true ) {
				var ok = true;
				try {
					s.connect(Const.HOST,Const.PORT);
				} catch( e : Dynamic ) {
					ok = false;
					neko.Lib.print("C");
					neko.Sys.sleep(0.1);
				}
				if( ok )
					break;
			}
			sockets.push(s);
		}
		var event = Math.round(Const.LOOPS / Const.SOCKETS);
		var t = neko.Sys.time();
		for( i in 0...Const.LOOPS ) {
			var c = i % 256;
			var k = Std.random(sockets.length);
			var s = sockets[k];
			s.output.writeChar(c);
			if( s.input.readChar() != c ) throw "???";
			if( k > 0 && k == sockets.length - 1 && Std.random(event) == 0 ) {
				neko.Lib.print("x");
				s.close();
				sockets.remove(s);
			}
		}
		time = neko.Sys.time() - t;
		for( s in sockets ) {
			neko.Lib.print("x");
			s.close();
		}
	}

}

class ServerSelect extends Task {

	var sockets : Array<neko.net.Socket>;

	public function new(size) {
		sockets = new Array();
		super();
	}

	public function addClient(sock) {
		t.sendMessage(sock);
	}

	function loop() {
		var s : neko.net.Socket = neko.vm.Thread.readMessage(true);
		sockets.push(s);
		while( true ) {
			for( s in neko.net.Socket.select(sockets,null,null,0.1).read ) {
				try {
					s.output.writeChar(s.input.readChar());
				} catch( e : Dynamic ) {
					neko.Lib.print("-");
					sockets.remove(s);
					s.close();
				}
			}
			while( true ) {
				var s : neko.net.Socket = neko.vm.Thread.readMessage(false);
				if( s == null ) {
					if( sockets.length != 0 )
						break;
					done = true;
					s = neko.vm.Thread.readMessage(true);
					done = false;
				}
				sockets.push(s);
				neko.Lib.print("+");
			}
		}
	}

}


class ServerEvents extends Task {

	var sockets : Array<neko.net.Socket>;
	var poll : neko.net.Poll;

	public function new(size) {
		sockets = new Array();
		poll = new neko.net.Poll(size);
		super();
	}

	public function addClient(sock) {
		t.sendMessage(sock);
	}

	function loop() {
		var toremove = new Array();
		while( true ) {
			poll.events(0.1);
			var i = 0;
			var idx = poll.readIndexes;
			while( true ) {
				var s = sockets[idx[i]];
				if( s == null )
					break;
				i++;
				try {
					s.output.writeChar(s.input.readChar());
				} catch( e : Dynamic ) {
					neko.Lib.print("-");
					toremove.push(s);
				}
			}
			if( toremove.length > 0 ) {
				for( s in toremove ) {
					sockets.remove(s);
					s.close();
				}
				poll.prepare(sockets,[]);
				toremove = new Array();
			}
			var mod = false;
			while( true ) {
				var s : neko.net.Socket = neko.vm.Thread.readMessage(false);
				if( s == null ) {
					if( sockets.length != 0 )
						break;
					done = true;
					s = neko.vm.Thread.readMessage(true);
					done = false;
				}
				sockets.push(s);
				mod = true;
				neko.Lib.print("+");
			}
			if( mod )
				poll.prepare(sockets,[]);
		}
	}

}


class MTNetwork {

	static var cputime : Void -> Float = neko.Lib.load("std","sys_cpu_time",0);

	static function main() {

		var size = Math.round(Const.CLIENTS * Const.SOCKETS / Const.SERVERS);
		trace("Starting on "+Const.HOST.toString()+":"+Const.PORT);
		trace("Socket per server "+size);

		var cpu = cputime();
		var time = neko.Sys.time();

		var sock = new neko.net.Socket();
		sock.bind(Const.HOST,Const.PORT);
		sock.listen(Const.CLIENTS * Const.SOCKETS);

		var servers = new Array<{ done : Bool, addClient : neko.net.Socket -> Void }>();
		for( i in 0...Const.SERVERS )
			if( Const.USE_POLL )
				servers.push(new ServerEvents(size));
			else
				servers.push(new ServerSelect(size));

		var clients = new Array();
		for( i in 0...Const.CLIENTS )
			clients.push(new Client());

		for( cid in 0...Const.CLIENTS * Const.SOCKETS ) {
			var s = sock.accept();
			s.setBlocking(false);
			servers[cid%servers.length].addClient(s);
		}

		var done = false;
		while( !done ) {
			done = true;
			for( c in clients )
				if( !c.done ) {
					done = false;
					break;
				}
			if( !done ) {
				neko.Sys.sleep(0.5);
				continue;
			}
			for( s in servers )
				if( !s.done ) {
					done = false;
					break;
				}
			neko.Sys.sleep(0.5);
		}

		var cpu2 = cputime();
		var time2 = neko.Sys.time();

		var st = 0.;
		var min = 1000.;
		var max = 0.;
		for( c in clients ) {
			st += c.time;
			if( c.time < min ) min = c.time;
			if( c.time > max ) max = c.time;
		}
		neko.Lib.print("\n");
		trace("AVG = "+(st / (Const.CLIENTS * Const.LOOPS)));
		trace("MIN = "+(min / Const.LOOPS));
		trace("MAX = "+(max / Const.LOOPS));
		trace("CPU = "+((cpu2 - cpu) / Const.LOOPS));
		trace("TOTAL TIME = "+((time2 - time) / Const.LOOPS));
		trace("DONE");
	}

}
