package unit.issues.misc;

import haxe.macro.Expr;
import haxe.macro.Context;
import haxe.macro.Type;

using haxe.macro.Tools;

abstract TypeName(String) to String {
	public inline function new(s:String) {
		this = s;
	}

	@:from static macro function fromE(e:Expr) {
		return macro new TypeName($v{Context.typeof(e).toString()});
	}
}

abstract Arity(Int) to Int {
	public inline function new(i:Int) {
		this = i;
	}

	@:from static macro function fromE(e:Expr) {
		return switch (Context.typeof(e).follow()) {
			case TFun(tl, _): macro new Arity($v{tl.length});
			case _: Context.error("Expected function type", e.pos);
		}
	}
}

abstract EnumListener<T>(String) to String {
	public inline function new(s:String) {
		this = s;
	}

	@:from static macro function fromE(e:Expr) {
		var e = Context.typeExpr(e);
		var ef = switch (e.expr) {
			case TField(_, FEnum(_, ef)): ef;
			case _: Context.error("Expected enum constructor", e.pos);
		}
		var t = switch (Context.getExpectedType().follow()) {
			case TAbstract(_, [t]): t;
			case _: Context.error("Something went wrong", e.pos);
		}
		var tvoid = Context.typeof(macro (null : Void));
		var t2 = switch (e.t.follow()) {
			case TFun(tl, _): TFun(tl, tvoid);
			case TEnum(_): TFun([], tvoid);
			case _: Context.error("Something went wrong, again", e.pos);
		}
		Context.unify(t, t2);
		return macro new EnumListener($v{ef.name});
	}
}