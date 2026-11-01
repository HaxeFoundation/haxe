import haxe.Constraints.Constructible;

@:generic
class Test<T:Constructible<Void->Void>> {

	var sleepAmount:Float = 0;

	public function new() {
		var fn = function() {
			Sys.sleep(this.sleepAmount);
		}
	}
}

class BoringClass {
	public function new() { }
}

class Main {
	static function main() {
		new Test<BoringClass>();
	}
}