package unit.issues;

#if macro
import haxe.macro.Context;
#end

private typedef A = {a:Int}
private typedef B = {b:Int}
private typedef C = {>A,}
private typedef D = {>A, >B,}

private typedef C2 = {} & A;
private typedef D2 = A & B;

class Issue3198 extends Test {
	function test() {
		eq(getExtends((null : C)), "A");
		eq(getExtends((null : D)), "A,B");
		eq(getExtends((null : C2)), "A");
		eq(getExtends((null : D2)), "A,B");
	}

	static macro function getExtends(e) {
		switch (Context.follow(Context.typeof(e))) {
			case TAnonymous(_.get() => {status: AExtend(_.get() => tl)}):
				var p = [];
				for (t in tl) switch (t) {
					case TType(_.get() => dt, []):
						p.push(dt.name);
					case TAnonymous(_):
						// ignore
					default:
						throw false;
				}
				return macro $v{p.join(",")};
			default:
				throw false;
		}
	}
}
