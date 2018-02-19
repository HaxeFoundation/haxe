class Main3 {
	public static function main() {
		var x = new Main3();
		trace(x.test());
	}
	final i = 0;
	function new() {
		i = 2;
	}
	function test():Int {return i;}
}
