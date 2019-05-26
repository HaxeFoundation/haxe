class Main {
	public static function main() {
		#if !macro
		connect();
		#end
	}

	macro static function connect() {
		try {
			new sys.net.Socket().connect(new sys.net.Host("192.168.0.66"), 443);
		} catch(e:String) {
			haxe.macro.Context.error(e, (macro {}).pos);
		}
		return macro {}
	}
}