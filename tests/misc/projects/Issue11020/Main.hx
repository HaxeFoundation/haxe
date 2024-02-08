#if !macro @:build(Main.error()) #end
class Main {
	var field:Int = 0;
	static function main() {}
}

#if macro
function error() {
	var fields = haxe.macro.Context.getBuildFields();
	haxe.macro.Context.info("Context.info", fields[0].pos);
	haxe.macro.Context.warning("Context.warning", fields[0].pos);
	haxe.macro.Context.error("Context.error", fields[0].pos);

	return null;
}
#end

