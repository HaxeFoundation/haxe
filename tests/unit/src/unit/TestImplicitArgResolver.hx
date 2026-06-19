package unit;

class TestImplicitArgResolver extends Test {
	// a plain (non-macro) resolver fills an omitted optional argument
	function testPlain() {
		eq("plain", plain());
		eq("given", plain("given"));
	}

	// a resolver may itself take ?pos:PosInfos, which forwards the original call site
	function testPosForwarding() {
		eq("testPosForwarding", withPos());
	}

	// a macro resolver runs at the call site (here: capturing the enclosing method)
	function testMacro() {
		eq("testMacro", withMacro());
	}

	// several implicit arguments (resolver + PosInfos) all fill independently
	function testMultiple() {
		eq("plain/testMultiple", multi());
		eq("plain/plain", two());
	}

	// a resolver-typed optional argument may sit before a rest argument
	function testBeforeRest() {
		eq("plain|1,2,3", beforeRest(1, 2, 3));
		eq("plain|", beforeRest());
	}

	static function plain(?c:Plain):String
		return (c : String);

	static function withPos(?c:PosCtx):String
		return (c : String);

	static function withMacro(?c:MacroCtx):String
		return (c : String);

	static function multi(?c:Plain, ?pos:haxe.PosInfos):String
		return (c : String) + "/" + pos.methodName;

	static function two(?a:Plain, ?b:Plain):String
		return (a : String) + "/" + (b : String);

	static function beforeRest(?c:Plain, ...rest:Int):String
		return (c : String) + "|" + rest.toArray().join(",");
}

@:implicitArgResolver(resolve)
private abstract Plain(String) from String to String {
	public inline function new(s:String)
		this = s;

	static function resolve():Plain
		return new Plain("plain");
}

@:implicitArgResolver(resolve)
private abstract PosCtx(String) to String {
	public inline function new(s:String)
		this = s;

	static function resolve(?pos:haxe.PosInfos):PosCtx
		return new PosCtx(pos.methodName);
}

@:implicitArgResolver(resolve)
private abstract MacroCtx(String) from String to String {
	macro static function resolve() {
		return macro $v{haxe.macro.Context.getLocalMethod()};
	}
}
