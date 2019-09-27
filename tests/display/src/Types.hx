typedef ToplevelElement = {
	kind:String,
	name:String,
	type:String,
	doc:String
}

typedef FieldElement = {
	name:String,
	type:String,
	kind:String,
	doc:String
}

abstract Position(Int) to Int {
	public inline function new(i:Int) {
		this = i;
	}
}
