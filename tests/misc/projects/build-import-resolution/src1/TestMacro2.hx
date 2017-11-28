import haxe.macro.*;
class TestMacro2 {

	public static function build (v:Int, isBuild:Bool) {
		isBuild ? Stats.add1(v) : Stats.add2(v);
		return Context.getBuildFields();
	}
}