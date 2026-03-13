import haxe.Json;
import sys.io.File;

class FormatterVersion {
	macro public static function getFormatterVersion():haxe.macro.Expr.ExprOf<String> {
		#if !display
		try {
			var content:String = File.getContent("haxelib.json");
			var haxelib = Json.parse(content);
			var version:String = haxelib.version;
			return macro $v{version};
		} catch (e:Any) {
			var version:String = "dev";
			return macro $v{version};
		}
		#else
		var version:String = "dev";
		return macro $v{version};
		#end
	}
}
