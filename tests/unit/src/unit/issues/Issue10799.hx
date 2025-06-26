package unit.issues;

class Issue10799 extends Test {
	#if lua
	var myField:(Int) -> Int;

	private static function foo(x:Int):Int
		return x * 3;

	private dynamic function bar(x:Int):Int {
		eq("table", untyped __lua__("_G.type(self)"));
		eq("number", untyped __lua__("_G.type(x)"));
		return x * 4;
	}

	private dynamic function baz(x:Int):Int {
		throw "not implemented";
	}

	private function returnsField():(Int) -> Int {
		return this.myField;
	}

	private function returnsMethod():(Int) -> Int {
		return this.bar;
	}

	private function returnsClosure():(Int) -> Int {
		final obj = lua.Lua.assert({num: 7});
		final fn:(Int) -> Int = x -> x * obj.num;
		lua.Lua.assert(fn);
		return fn;
	}

	public function test() {
		this.myField = x -> x * 2;
		eq(6, untyped __lua__("self.myField(3)"));
		this.myField = Issue10799.foo;
		eq(9, untyped __lua__("self.myField(3)"));
		eq(9, untyped __lua__("__unit_issues_Issue10799.foo(3)"));
		this.myField = this.bar;
		eq(12, untyped __lua__("self.myField(3)"));
		exc(() -> untyped __lua__("self.bar(3)"));
		this.myField = x -> x * 2;
		this.bar = this.myField;
		eq(6, untyped __lua__("self:bar(3)"));
		this.baz = this.bar;
		eq(6, untyped __lua__("self:baz(3)"));

		var localVar = this.myField;
		lua.Lua.assert(localVar);
		eq(6, untyped __lua__("localVar(3)"));

		localVar = this.bar;
		lua.Lua.assert(localVar);
		eq(6, untyped __lua__("localVar(3)"));

		final field = this.returnsField();
		eq(6, untyped __lua__("field(3)"));
		final method = this.returnsMethod();
		eq(6, untyped __lua__("method(3)"));
		final closure = this.returnsClosure();
		eq(21, untyped __lua__("closure(3)"));

		final anon = lua.Lua.assert({
			fromField: this.myField,
			fromStatic: Issue10799.foo,
			fromMethod: this.bar,
		});

		exc(() -> untyped __lua__("anon.fromField(3)"));
		eq(6, untyped __lua__("anon:fromField(3)"));
		exc(() -> untyped __lua__("anon.fromStatic(3)"));
		eq(9, untyped __lua__("anon:fromStatic(3)"));
		exc(() -> untyped __lua__("anon.fromMethod(3)"));
		eq(6, untyped __lua__("anon:fromMethod(3)"));
	}
	#end
}
