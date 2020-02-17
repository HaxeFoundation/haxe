package unit.issues;

#if js

private typedef PathSimple = haxe.extern.EitherType<String,js.lib.RegExp>;
private typedef Path = haxe.extern.EitherType<PathSimple,Array<PathSimple>>;

#end

class Issue3499 extends Test {
	#if js
	function test() {
		var a:Path = ["", new js.lib.RegExp("")];
		eq("", a[0]);
	}
	#end
}