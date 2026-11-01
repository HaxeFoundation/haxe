import haxe.rtti.Meta;
using Lambda;


class MainSub {}

class Main {
	static function main() {
		function hf(t, s) {
			Sys.stderr().writeString('$s: ${Reflect.hasField(t, s)}\n');
		}

		final numMetas = 6;

		var check = [
			{ name: "Main", meta: Meta.getType(Main) },
			{ name: "MainSub", meta: Meta.getType(MainSub) }
		];

		for (item in check) {
			Sys.stderr().writeString(item.name + "\n");
			for (i in 0...numMetas) {
				hf(item.meta, 'meta$i');
			}
		}
	}
}