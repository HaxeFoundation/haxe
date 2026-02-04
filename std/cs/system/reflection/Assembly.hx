package cs.system.reflection;

@:native("System.Reflection.Assembly")
extern class Assembly {
	static function GetExecutingAssembly():Assembly;
	var Location(default, never):String;
}
