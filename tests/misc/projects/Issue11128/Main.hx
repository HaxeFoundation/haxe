#if macro
import haxe.macro.Compiler;
import haxe.macro.Context;
#end

function main() {
	trace("Hello world!");
}

#if macro
function setupHxTestTarget() {
	// Check that output is available by --macro phase.
	// Good place for "custom target" to ensure output path is valid early on (file vs folder).
	switch(Compiler.getOutput()) {
		case "": Sys.println("no output");
		case o: Sys.println("output: " + o);
	}

	Context.onGenerate(onGenerate);
}

// Where "custom target" files would be generated.
function onGenerate(types: Array<haxe.macro.Type>) {
	// Make sure the config has not been overwritten since Init.init().
	final intended = Std.string(hxtest.Init.intendedConfig);
	final currentConfig = Std.string(Compiler.getConfiguration().platformConfig);
	Sys.println("[Generate] Config correct: " + (intended == currentConfig));
}
#end