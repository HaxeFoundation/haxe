import haxe.Constraints.FlatEnum;
import haxe.Constraints.Object;

enum E {
	A;
	B(s:String);
}

abstract AbstractOverClass(Array<Int>) {}
abstract AbstractOverNullClass(Null<Array<Int>>) {}

enum abstract EnumAbstractOverString(String) {
	var X = "x";
}

class C {}

@:coreType @:semantics(object) abstract CoreObject {}

class R<T:Object> {
	public function new() {}
}

class FromEmptyStructure<T:{}> {
	var r:R<T>;
}

class FromObject<T:Object> {
	var r:R<T>;
}

class FromEnumValue<T:EnumValue> {
	var r:R<T>;
}

class FromFlatEnum<T:FlatEnum> {
	var r:R<T>;
}

class FromClass<T:C> {
	var r:R<T>;
}

function main() {
	// everything the { } constraint accepts
	new R<C>();
	new R<Null<C>>();
	new R<String>();
	new R<Array<Int>>();
	new R<{x:Int}>();
	new R<{}>();
	new R<Dynamic>();
	// enums
	new R<E>();
	new R<Null<E>>();
	new R<EnumValue>();
	new R<haxe.ds.Option<Int>>();
	// abstracts over reference types
	new R<AbstractOverClass>();
	new R<AbstractOverNullClass>();
	new R<EnumAbstractOverString>();
	new R<Map<String, Int>>();
	// Dynamic and abstracts over it
	new R<Any>();
	// core types with object semantics
	new R<CoreObject>();
	// types of types
	new R<Class<C>>();
	new R<Enum<E>>();
	// AtomicObject itself
	#if target.threaded
	new haxe.atomic.AtomicObject<Null<E>>(null);
	#end
}
