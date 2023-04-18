typedef Foo = {
	final id:String;
	final ?project:String;
}

typedef Bar = {
	final id:String;
	final createDate:Date;
	final ?project:String;
}

class Main {
	static function constrained<T,R:T>(v:R):T
		return null;

	static function fn<T>(s:T):T {
		return s;
	}

	static function main() {
		var A:Int = fn('s'); // Error "String should be Int"
		var B:Int = constrained('s'); //Error "Int should be String" instead of "String should be Int"
		var C:Bar = constrained((null:Foo)); // compiles, but should error "Foo should be Bar; { ?project : Null<String>, id : String } has no field createDate"
	}
}