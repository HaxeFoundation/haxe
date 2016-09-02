typedef ToplevelElement = {
	kind: String,
	name: String
}

typedef FieldElement = {
	name: String,
	type: String
}

abstract Position(Int) to Int {
	public inline function new(i:Int) {
		this = i;
	}
}