class Test {
	var value:String;
	function new(val:String) {
 		value = val;
	}
	public static function main() {
		Sys.println(new Test("Hallo, wurld.").value);
	}
}