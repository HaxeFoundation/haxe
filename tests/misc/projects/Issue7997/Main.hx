import haxe.display.JsonModuleTypes;

class Main {
	static function main() {}

	function guessName<T>(type1:Null<JsonType<T>>):Null<String> {
		function loop(type) {
			return switch (type.kind) {
				case TInst: type.args.path.typeName;
				case TDynamic: loop(type.args);
				case _: null;
			}
		}
		return null;
	}
}