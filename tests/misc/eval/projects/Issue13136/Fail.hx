abstract AbstractOverInt(Int) {}

enum abstract EnumAbstractOverInt(Int) {
	var Y = 1;
}

class R<T:haxe.Constraints.ReferenceType> {
	public function new() {}
}

class FromUnconstrained<T> {
	var r:R<T>;
}

class FromInt<T:Int> {
	var r:R<T>;
}

function main() {
	new R<Int>();
	new R<Float>();
	new R<Bool>();
	new R<Null<Int>>();
	new R<AbstractOverInt>();
	new R<EnumAbstractOverInt>();
	new R<Any>();
	new R<() -> Void>();
	new R<haxe.Constraints.Function>();
}
