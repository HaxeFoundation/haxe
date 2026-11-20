// the value of a core type, used as a value or caught
@:build(Macro.statics(600))
class UseDynamic {
	public static function a0(x:Dynamic):Bool
		return Std.isOfType(x, Dynamic);
}

class CatchAll {
	public static function run(arr:Array<Int>) {
		var x = Macro.long(arr, 3000);
		try {
			throw 'a';
		} catch (e:haxe.Exception) {
			x++;
		}
		return x;
	}
}
