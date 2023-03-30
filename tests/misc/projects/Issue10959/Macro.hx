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

		// Use TypeTools.resolveTypeParameters
		final valid = type.resolveTypeParameters(true, (tp,t,i) -> tp.defaultType ?? tp.t);

		haxe.macro.Context.followWithAbstracts(valid);
	}

	// Test specific aspects of the resolveTypeParameters function
	static function testWithClass(c: Ref<ClassType>) {
		// Various types to use with tests
		final voidType = haxe.macro.Context.getType("Void");
		final intType = haxe.macro.Context.getType("Int");
		final floatType = haxe.macro.Context.getType("Float");
		final stringType = haxe.macro.Context.getType("String");

		// Print type before and after using "resolveTypeParameters"
		function print(t:Type, other:Null<Type> = null) {
			Sys.println("Before: " + t.toString());

			// Automatically resolve to the defaultType or `KTypeParameter` for testing purposes.
			if(other == null) {
				other = t.resolveTypeParameters(true, (tp,t,i) -> tp.defaultType ?? tp.t);
			}
			Sys.println("After:  " + other.toString());

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

		Sys.println("-- Fill With Specific Type --");
		
		print(inner1, inner1.resolveTypeParameters(true, (_,_,_) -> intType));
		print(inner1, inner1.resolveTypeParameters(true, (_,_,_) -> stringType));
		print(inner2, inner2.resolveTypeParameters(true, (_,_,_) -> stringType));

		Sys.println("-- Recursive Param OFF --");

		final recursiveType = TInst(c, [inner1, inner2]);
		print(recursiveType, recursiveType.resolveTypeParameters(false, (_,_,_) -> stringType));

		Sys.println("-- Index Print --");

		recursiveType.resolveTypeParameters(true, function(tp, type, index) {
			Sys.println(type.toString() + " - " + index);
			return voidType;
		});
	}
}

#end
