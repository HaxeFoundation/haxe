import haxe.Constraints;

class Main2 {
	static function main() {}
}

class GenericTest<T:(Main & Constructible<String->Void>)> {
	public function new(){}

	public function someTask():Void {
		var instance:T = getInstance("foo");
	}

	@:generic
	private function getInstance<S:(Main & Constructible<String->Void>)>(arg:String):S {
		return new S(arg);
	}
}

