class Main {
	public static function main() {
		#if !macro
		connect();
		#end
	}

	macro static function connect() {
		try {
			new sys.net.Socket().connect(new sys.net.Host("127.0.0.1"), 9999);
		} catch(e:String) {
			haxe.macro.Context.error('connection error', (macro {}).pos);
		}
		return macro {}
	}
}