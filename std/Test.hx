class Test {
	static function main() {
		for(i in 5...500)
			Sys.println(Std.string(i));
		var a = [5, 6, 7, 8];
		for(i in a)
			Sys.println(Std.string(i*i));
		var v = 0;
		while(true)
			if(++v > 2)
				break;

	}
}
interface TestInterface {
	var objs:Array<Dynamic>;
	function isAwesome():Int;
	var obj:Dynamic;
}