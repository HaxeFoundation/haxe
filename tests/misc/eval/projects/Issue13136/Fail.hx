abstract AbstractOverInt(Int) {}

@:semantics(value) class ValueClass {}

@:semantics(variable) class VariableClass {}

@:semantics(reference) typedef Ref<T> = T;

abstract AbstractOverValueClass(ValueClass) {}

enum abstract EnumAbstractOverInt(Int) {
	var Y = 1;
}

class R<T:haxe.Constraints.Object> {
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
	new R<() -> Void>();
	new R<ValueClass>();
	new R<VariableClass>();
	new R<Ref<Array<Int>>>();
	new R<AbstractOverValueClass>();
}
