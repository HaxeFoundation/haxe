extern class J {
	@:overload(function(s:String):Void {})
	public function new():Void;
}

class Main {
	static function main() {
		new J(my_typo);
	}
}
