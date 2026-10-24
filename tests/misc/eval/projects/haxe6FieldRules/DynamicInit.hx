// A variable initialized with a Dynamic expression has no inferred type: its
// first use does not bind it any more, so it needs a type annotation.
class DynamicInit {
	static var dyn = (null : Dynamic);

	static function main() {
		if (dyn != null) dyn(1);
	}
}
