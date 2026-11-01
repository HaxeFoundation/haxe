class Main {
	static function main() {
		var c = Std.int(Math.random() - 0.5); //0

		Sys.stderr().writeString(switch (c) {
			case 1: '1';
			default: 'analyzer removed this ';
		});
	}
}