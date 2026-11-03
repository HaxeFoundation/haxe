package;

#if macro
import haxe.macro.Compiler;
import haxe.macro.Compiler.PackageRule;
#end

function main() {
}

#if macro
class MacroClass {
	public static function start() {
		final config = Compiler.getConfiguration();

		trace(filterArgs(config.args));
		trace(config.debug);
		trace(config.verbose);
		trace(config.foptimize);
		trace(platformToString(config.platform));
		trace(config.mainClass.pack);
		trace(config.mainClass.name);

		for(packageName => packageRule in config.packageRules) {
			switch(packageRule) {
				case Forbidden: trace(packageName + " is forbidden");
				case _:
			}
		}
	}

	static function platformToString(p: Platform) {
		return switch(p) {
			case Eval: "eval";
			case Js: "js";
			case Cpp: "cpp";
			case _: "unused platform";
		}
	}

	static function filterArgs(args:Array<String>):Array<String> {
		if (args.length < 2) return args;
		// We're currently prepending that to all tests while moving to pretty errors by default
		if (args[0] == "-D" && args[1] == "message.reporting=classic") return args.slice(2);
		return args;
	}
}
#end
