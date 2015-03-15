package unit.issues;

class Issue2629 extends unit.Test {

	public function test() {
		#if !macro
		var d:Int = expectedType("Int");
		var d:String = expectedType("String");
		var d = expectedType("Unknown<0>");
		var d:Dynamic = expectedType("Dynamic");
		var d:Dynamic<String> = expectedType("Dynamic<String>");
		#end
	}

	public function testTypeParameter<T>() {
		var d:T = expectedType("testTypeParameter.T");
	}

	macro static function expectedType(s:String) {
		var t = haxe.macro.Context.getExpectedType();
		return macro {
			eq($v{haxe.macro.TypeTools.toString(t)}, $v{s});
			(null : Dynamic);
		}
	}
}