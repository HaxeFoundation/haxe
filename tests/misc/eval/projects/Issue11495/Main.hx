import haxe.macro.Context;
import haxe.macro.TypeTools;

class Main {
	static function main() {
		check();
	}

	static macro function check() {
		var int = Context.getType("Int");
		var t = try {
			TypeTools.applyTypeParameters(int, [{name: "T", t: Context.getType("String")}], [int]);
		} catch (e:String) {
			if (!StringTools.startsWith(e, "Unexpected type where type parameter was expected")) {
				Context.error('Unexpected exception: $e', Context.currentPos());
			}
			return macro null;
		}
		Context.error('Expected an exception, got ${TypeTools.toString(t)}', Context.currentPos());
		return macro null;
	}
}
