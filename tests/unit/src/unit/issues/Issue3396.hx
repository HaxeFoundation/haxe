package unit.issues;

class Issue3396 extends Test {
	function test() {
		var r = null;
		var f = function(arr:Array<Dynamic>) return r;
		var f2 = function(arr:Array<Dynamic>):Void {}
		Reflect.makeVarArgs(f);
		Reflect.makeVarArgs(f2);
		var s:String = f([]);
		#if cs
		unit.HelperMacros.typedAs(r, (r : String));
		#else
		unit.HelperMacros.typedAs(r, (r : Null<String>));
		#end
		unit.HelperMacros.typedAs(f2([]), (null : Void));
	}
}
