extern class Ext {
	function new():Void;
	overload function f(s:String):Void;
	overload function f(e:Ext):Void;
}

class Main {
	static function main() {
		var m = new Ext();
		m.f(null);
	}
}