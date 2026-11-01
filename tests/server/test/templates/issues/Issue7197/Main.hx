class B extends A {
	public function new() {
		super();
		// The struct is passed for the trailing optional `opt`, skipping `alias`
		// and `addToFxs`. Typing it against `alias:String` first must not leak a
		// false-positive "Unresolved identifier FBone" diagnostic.
		addFx("Sparkles01", {parent: FBone("FX_Sparkles01")});
	}
}

class Main {
	static function main() {
		new B();
	}
}
