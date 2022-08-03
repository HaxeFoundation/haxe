class Main {
	static function main() {
		var data = haxe.io.Bytes.alloc(2);
		data.set(0, 0xFF);
		data.toString().substr(0);
	}
}
