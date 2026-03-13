package safety.macro;

#if !macro
@:genericBuild(safety.macro.ArgumentNullCheck.NullCheckBuilder.build())
class ArgumentNullCheck<T> {}

abstract GenericCheck<T>(Null<T>) {
	public inline function new(value:T, type:String, method:String, argument:String) {
		this = value;
		if(this == null) {
			throw new IllegalArgumentException('Null is not allowed for argument $argument in $type.$method()');
		}
	}
}

abstract NoCheck(Bool) {
	public inline function new(value:Any, type:String, method:String, argument:String) {
		this = false;
	}
}
#else

import haxe.macro.Expr;
import haxe.macro.Context;
import haxe.macro.Type;

using haxe.macro.Tools;

@:enum
abstract Nullability(Int) {
	/** Indicates if `Null<>` is used explicitly */
	var Explicit = 1;
	/** Indicates if a type implies assigning `null` is possible */
	var Implicit = 2;
	/** Null cannot be assigned to a type (e.g. Int on static target) */
	var No = 0;
}

class NullCheckBuilder {
	macro static public function build():ComplexType {
		switch(Context.getLocalType()) {
			case TInst(_.toString() => 'safety.macro.ArgumentNullCheck', [type]):
				switch(getNullability(type)) {
					case Implicit:
						var complexType = type.toComplexType();
						if(complexType == null) {
							complexType = macro:safety.macro.Monomorph<0>;
						}
						return macro:safety.macro.ArgumentNullCheck.GenericCheck<$complexType>;
					case _:
						return macro:safety.macro.ArgumentNullCheck.NoCheck;
				}
			case _:
				return macro:safety.macro.ArgumentNullCheck.NoCheck;
		}
	}

	static function getNullability(type:Type, depth:Int = 0):Nullability {
		//guard against infinite monomorph resolution
		if(depth > 10) {
			return Implicit;
		}
		return
			switch (type) {
				case null:
					Implicit;
				case TMono(_.get() => t):
					getNullability(type, depth + 1);
				case TLazy(f):
					getNullability(f());
				case TType(_, _):
					getNullability(Context.follow(type, true));
				case TAbstract(_.toString() => fqn, _) if(fqn == 'Null' || fqn == 'StdTypes.Null'):
					Explicit;
				case TAbstract(_.get() => a, _):
					a.meta.has(':notNull')
						? (Context.defined('static') ? No : Implicit)
						: (a.meta.has(':coreType') ? Implicit : getNullability(a.type));
				case _:
					Implicit;
			}
	}
}

#end