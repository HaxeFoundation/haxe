package unit.issues;

import unit.HelperMacros.typeError;

private abstract FromStruct({id:Int}) from {id:Int} {
	public var id(get, never):Int;

	inline function get_id()
		return this.id;
}

private abstract FromStringId({id:String}) from {id:String} {}

private abstract FromMethod(Int) {
	inline function new(v:Int)
		this = v;

	@:from static inline function ofStruct(v:{id:Int}):FromMethod
		return new FromMethod(v.id);

	public inline function toInt()
		return this;
}

private abstract Box<T>(T) from T {}

class Issue11068 extends Test {
	function test() {
		final data = ({} : Dynamic);
		data.id = 0;
		use(data);
		eq(0, data.id);

		final openAfter = ({} : Dynamic);
		openAfter.id = 1;
		use(openAfter);
		openAfter.name = "test";
		eq(1, openAfter.id);
		eq("test", openAfter.name);

		final nullable = ({} : Dynamic);
		nullable.id = 2;
		useNull(nullable);
		eq(2, nullable.id);

		final structData = ({} : Dynamic);
		structData.id = 3;
		structData.extra = "ok";
		eq(3, useFromStruct(structData));
		eq(3, structData.id);
		eq("ok", structData.extra);

		final methodData = ({} : Dynamic);
		methodData.id = 4;
		eq(4, useFromMethod(methodData));
		eq(4, methodData.id);

		final boxed = ({} : Dynamic);
		boxed.id = 5;
		useBox(boxed);
		eq(5, boxed.id);

		var constrained = makeConstrained();
		use(constrained);
		constrained = {id: 6};
		eq(6, constrained.id);

		final invalid = ({} : Dynamic);
		invalid.id = 0;
		t(typeError(useFromStringId(invalid)));
	}

	static function use(v:Any) {}

	static function useNull(v:Null<Any>) {}

	static function useFromStruct(v:FromStruct)
		return v.id;

	static function useFromStringId(v:FromStringId) {}

	static function useFromMethod(v:FromMethod)
		return v.toInt();

	static function useBox<T>(v:Box<T>) {}

	static function makeConstrained<T:{id:Int}>():T
		return cast {id: 6};
}
