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

		trace(config.args);
		trace(config.debug);
		trace(config.verbose);
		trace(config.foptimize);
		trace(config.platform);
		trace(config.mainClass.pack);
		trace(config.mainClass.name);

		for(packageName => packageRule in config.packageRules) {
			switch(packageRule) {
				case Forbidden: trace(packageName + " is forbidden");
				case _:
			}
		}
	}
}
#end
