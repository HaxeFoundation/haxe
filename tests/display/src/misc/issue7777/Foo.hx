package misc.issue7777;

#if (eval || macro)
import haxe.macro.Expr;
#end

abstract Foo<T>(T) {
	@:from public static macro function fromThing<T>(e:ExprOf<Thing>):ExprOf<Foo<T>> {
		return macro null;
	}
}
