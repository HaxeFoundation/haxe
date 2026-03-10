class Issue6276 {
	public static function main() {
		var s = "foo";
		var indexOf = Reflect.field(s, "indexOf");
		var isfunc = lua.Lua.type(indexOf) == "function";
		if (!isfunc){
			trace("DCE should not remove string functions in lua");
			Sys.exit(1);
		}
		var res = Reflect.callMethod(s, indexOf, ["o"]);
		if (res != 1){
			trace("Something went wrong with calling string methods via reflect");
			Sys.exit(1);
		}
		var eq = Reflect.compareMethods(Reflect.field(s, 'indexOf'), Reflect.field(s, 'indexOf'));
		if (!eq){
			trace("Reflect.compareMethods should work for string methods");
			Sys.exit(1);
		}
	}
}
