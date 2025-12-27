@:cppFileCode(
"static void native(int a) {}"
)
class Main {
	@:native("native")
	static function native(a:Int):Void {}
}
