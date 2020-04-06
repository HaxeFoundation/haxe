class Main {
	function new(a:Int) {}

	static function main() {
		var f = Main.new.bind("not an int");
	}
}