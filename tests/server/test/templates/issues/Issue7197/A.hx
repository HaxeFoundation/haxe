enum FxAttach {
	FBone(?name:String);
}

typedef FxOptions = {
	?parent:FxAttach,
}

class A {
	public function new() {}

	public function addFx(name:String, ?alias:String, ?addToFxs = true, ?opt:FxOptions) {}
}
