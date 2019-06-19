class Main {
	@:analyzer(ignore)
	static function main() {
		Std.downcast((null:One), Two);
		Std.downcast((null:IOne), ITwo);
	}
}

class One {}
class Two {}

interface IOne {}
interface ITwo {}
