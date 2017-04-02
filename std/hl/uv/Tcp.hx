package hl.uv;

@:hlNative("uv")
class Tcp extends Stream {

	public function new( ?loop : Loop ) {
		if( loop == null ) loop = Loop.getDefault();
		super(tcp_init_wrap(loop));
	}

	public function connect( host : sys.net.Host, port : Int, onConnected : Bool -> Void ) {
		var h = tcp_connect_wrap(handle, host.ip, port, onConnected);
		if( h == null ) throw haxe.io.Error.Custom("Failed to connect to "+host+":"+port);
	}

	public function bind( host : sys.net.Host, port : Int ) {
		if( !tcp_bind_wrap(handle, host.ip, port) )
			throw haxe.io.Error.Custom("Failed to bind socket to "+host+":"+port);
	}

	public function accept() {
		var client = handle == null ? null : tcp_accept(handle);
		if( client == null ) throw new haxe.io.Eof();
		return new Stream(client);
	}

	static function tcp_init_wrap( loop : Loop ) : HandleData {
		return null;
	}

	static function tcp_connect_wrap( h : HandleData, host : Int, port : Int, onConnected : Bool -> Void ) : HandleData {
		return null;
	}

	static function tcp_bind_wrap( h : HandleData, host : Int, port : Int ) : Bool {
		return false;
	}

	static function tcp_accept( h : HandleData ) : HandleData {
		return null;
	}

}