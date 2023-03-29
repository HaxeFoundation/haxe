package;

#if macro

import haxe.macro.Type;

using haxe.macro.TypeTools;

class Macro {
	public static function start() {
		haxe.macro.Context.onGenerate(function(types: Array<haxe.macro.Type>) {
			// Make sure this actually runs.
			Sys.println("On Generate");

			for(t in types) {
				switch (t) {
					case TAbstract(a, _): {
						makeMalformedType(a);
					}
					case TInst(c, _) if(c.get().name == "TestClass"): {
						testWithClass(c);
					}
					case _: {}
				}
			}
		});
	}

	// General test that ensures crash doesn't occur
	static function makeMalformedType(a: Ref<AbstractType>) {
		// If passed as it is, this will crash the Haxe compiler.
		// https://github.com/HaxeFoundation/haxe/issues/10959
		final type = TAbstract(a, []);

		// Use TypeTools.validateTypeParams
		final valid = type.validateTypeParams();

		haxe.macro.Context.followWithAbstracts(valid);
	}

	// Test specific aspects of the validateTypeParams function
	static function testWithClass(c: Ref<ClassType>) {
		// Various types to use with tests
		final voidType = haxe.macro.Context.getType("Void");
		final intType = haxe.macro.Context.getType("Int");
		final floatType = haxe.macro.Context.getType("Float");
		final stringType = haxe.macro.Context.getType("String");
		
		// Print type before and after using "validateTypeParams"
		function print(t:Type) {
			Sys.println("Before: " + t.toString());
			Sys.println("After:  " + t.validateTypeParams().toString());
			Sys.println("");
		}

		Sys.println("-- Too Few Params --");

		print(TInst(c, []));
		print(TInst(c, [voidType]));

		Sys.println("-- Too Many Params --");

		print(TInst(c, [voidType, intType, floatType, stringType]));

		Sys.println("-- Correct Number of Params (3rd is optional) --");

		print(TInst(c, [voidType, intType, floatType]));
		print(TInst(c, [voidType, intType]));

		Sys.println("-- Shouldn't Have Params --");

		switch(voidType) {
			case TAbstract(absRef, _): {
				print(TAbstract(absRef, [intType, floatType]));
			}
			case _:
		}

		Sys.println("-- Recursive Test --");

		final inner1 = TInst(c, []);
		final inner2 = TInst(c, [voidType, intType, floatType, stringType]);
		print(TInst(c, [inner1, inner2]));
	}
}

#end
