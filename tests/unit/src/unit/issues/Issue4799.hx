package unit.issues;

import unit.HelperMacros.typeString;

class Issue4799 extends Test {
	function test() {
		eq(typeString((null : Int)), "Int");
		eq(typeString((null : (Int))), "Int");
		eq(typeString((null : (Int,Int)->Int)), "Int -> Int -> Int");
		eq(typeString((null : (a:Int,Int)->Int)), "a : Int -> Int -> Int");
		eq(typeString((null : (a:Int, b:Int)->Int)), "a : Int -> b : Int -> Int");
		eq(typeString((null : (a:Int, Int, ?String->Void, ?b:(Int->Int)->Int, c:(a:Int)->Void)->Int)),
		                      "a : Int -> Int -> (?String -> Void) -> ?b : ((Int -> Int) -> Int) -> c : (a : Int -> Void) -> Int");
		t(parseError("(Int,Int)->Int->Int"));
		t(parseError("(a:Int)->Int->Int"));
		eq(typeString((null : (Int,Int)->(Int->Int))), "Int -> Int -> (Int -> Int)");
		eq(typeString((null : (a:Int)->(Int->Int))), "a : Int -> (Int -> Int)");
	}

	static macro function parseError(s:String) {
		var s = "(null : " + s + ")";
		try {
			haxe.macro.Context.parseInlineString(s, haxe.macro.Context.currentPos());
			return macro false;
		} catch (e:Any) {
			return macro true;
		}
	}
}
