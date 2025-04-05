import haxe.extern.EitherType as Or;

typedef Callback = Or<
		Or<
			() -> Void,
			(a:Int) -> Void
		>,
		(a:Int, b:Int) -> Void
	>;

class Main {
	static function main() {
		final foo:Callback = a -> {
			$type(a); // Unknown<0> instead of Int
			return;
		}
	}
}