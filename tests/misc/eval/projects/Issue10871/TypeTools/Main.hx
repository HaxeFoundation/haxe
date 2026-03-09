package;

#if macro
import haxe.macro.Context;
import haxe.macro.TypeTools;
import haxe.macro.Type;
#end

function main() {
}

#if macro
class MacroClass {
	public static function start() {
		Context.onGenerate(function(types: Array<Type>) {
			for(t in types) {
				testTypeConversion(t);
			}
		});
	}

	static function testTypeConversion(t: Type) {
		final moduleType = TypeTools.toModuleType(t);

		switch(t) {
			case TInst(t, params): {
				switch(moduleType) {
					case TClassDecl(t2): {
						if(!compareClassTypes(t.get(), t2.get())) {
							trace("Class types not equal");
						}
					}
					case _: trace("Module wasn't class type");
				}
			}
			case TEnum(e, params): {
				switch(moduleType) {
					case TEnumDecl(e2): {
						if(!compareEnumTypes(e.get(), e2.get())) {
							trace("Enum types not equal");
						}
					}
					case _: trace("Module wasn't enum type");
				}
			}
			case TType(d, params): {
				switch(moduleType) {
					case TTypeDecl(d2): {
						if(!compareDefTypes(d.get(), d2.get())) {
							trace("Def types not equal");
						}
					}
					case _: trace("Module wasn't def type");
				}
			}
			case TAbstract(a, params): {
				switch(moduleType) {
					case TAbstract(a2): {
						if(!compareDefTypes(a.get(), a2.get())) {
							trace("Abstract types not equal");
						}
					}
					case _: trace("Module wasn't abstract type");
				}
			}
			case _:
		}
	}

	static function compareClassTypes(c1: ClassType, c2: ClassType): Bool {
		return (
			c1.name == c2.name &&
			c1.module == c2.module &&
			c1.pack.toString() == c2.pack.toString() &&
			Std.string(c1.pos) == Std.string(c2.pos) &&
			c1.isPrivate == c2.isPrivate &&
			c1.isInterface == c2.isInterface &&
			c1.isFinal == c2.isFinal &&
			c1.isExtern == c2.isExtern &&
			c1.isAbstract == c2.isAbstract
		);
	}

	static function compareEnumTypes(e1: EnumType, e2: EnumType): Bool {
		return (
			e1.name == e2.name &&
			e1.module == e2.module &&
			e1.pack.toString() == e2.pack.toString() &&
			e1.names.toString() == e2.names.toString() &&
			Std.string(e1.pos) == Std.string(e2.pos) &&
			e1.isPrivate == e2.isPrivate &&
			e1.isExtern == e2.isExtern
		);
	}

	static function compareDefTypes(d1: DefType, d2: DefType): Bool {
		return (
			d1.name == d2.name &&
			d1.module == d2.module &&
			d1.pack.toString() == d2.pack.toString() &&
			Std.string(d1.pos) == Std.string(d2.pos) &&
			d1.isPrivate == d2.isPrivate &&
			d1.isExtern == d2.isExtern
		);
	}

	static function compareAbstractTypes(a1: AbstractType, a2: AbstractType): Bool {
		return (
			a1.name == a2.name &&
			a1.module == a2.module &&
			a1.pack.toString() == a2.pack.toString() &&
			Std.string(a1.pos) == Std.string(a2.pos) &&
			a1.isPrivate == a2.isPrivate &&
			a1.isExtern == a2.isExtern
		);
	}
}
#end
