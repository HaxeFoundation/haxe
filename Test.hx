class Test {
	var b:Null<Int>=23;
	var c:String="No idea";
	function new() {

	}
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
		var func = function(a, b) return '${a} ${b*100}';
		Sys.println(func("Hello", 3));
		try {
			throw "Random error";
		} catch(error:String) {
			Sys.println(error);
		}
	}
}
interface TestInterface {
	var objs:Array<Dynamic>;
	var awesomeness(get, set):Float;
	function isAwesome():Int;
	var obj:Dynamic;
}