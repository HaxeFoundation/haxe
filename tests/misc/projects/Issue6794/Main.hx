#if macro
import haxe.macro.Expr;
import haxe.macro.Context;
#end

class Main {
	#if !macro
	static function main()
		test();
	#end

	static macro function test() {
		try Context.typeof(macro foo)
		catch (e) Context.warning("foo", (macro 0).pos);
		return macro {};
	}
}
