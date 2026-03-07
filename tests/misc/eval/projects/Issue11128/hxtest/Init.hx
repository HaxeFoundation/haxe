package hxtest;

import haxe.macro.Compiler;
import haxe.macro.PlatformConfig;

class Init {
	// Default, except everything has been set to `true`.
	public static var intendedConfig: PlatformConfig = {
		supportsAtomics: true,
		thisBeforeSuper: true,
		scoping: {
			scope: BlockScope,
			flags: []
		},
		exceptions: {
			nativeThrows: [],
			avoidWrapping: true,
			baseThrow: {
				name: "Dynamic",
				pack: ["StdTypes"]
			},
			nativeCatches: [],
			wildcardCatch: {
				name: "Dynamic",
				pack: ["StdTypes"]
			}
		},
		supportsRestArgs: true,
		overloadFunctions: true,
		capturePolicy: None,
		staticTypeSystem: true,
		supportsUnicode: true,
		supportsFunctionEquality: true,
		reservedTypePaths: [],
		addFinalReturn: true,
		supportsThreads: true,
		sys: true,
		usesUtf16: true,
		padNulls: true
	}

	public static function init() {
		Sys.println("hxtest.Init.init()");
		haxe.macro.Compiler.setPlatformConfiguration(intendedConfig);

		// Check the config that was just set.
		final intended = Std.string(intendedConfig);
		final currentConfig = Std.string(Compiler.getConfiguration().platformConfig);
		Sys.println("[Init] Config correct: " + (intended == currentConfig));
	}
}
