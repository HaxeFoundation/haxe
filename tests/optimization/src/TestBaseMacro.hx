import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;

using StringTools;
using haxe.macro.Tools;

class TestBaseMacro {
	static var numFailures = 0;

	macro static public function run() {
		var c = Context.getLocalClass();
		var fields = c.get().fields.get();
		var acc = [];
		var eSetup = macro setup();
		for (field in fields) {
			if (field.name.startsWith("test")) {
				acc.push(eSetup);
				acc.push(macro $i{field.name}());
			}
		}
		acc.push(macro {
			trace("Done " +numTests+ " tests (" +numFailures+ " failures)");
			if(numFailures > 0) {
				Sys.exit(1);
			}
		});
		Context.onGenerate(check);
		return macro $b{acc};
	}

	#if macro
	static function check(types:Array<Type>) {
		for (t in types) {
			switch (t) {
				case TInst(c = _.get() => { superClass: { t: _.get().name => "TestBase" }},_):
					checkClass(c.get());
				case _:
			}
		}
		if (numFailures > 0) {
			Sys.exit(1);
		}
	}

	static function checkClass(c:ClassType) {
		for (field in c.fields.get()) {
			checkExpr(field.expr());
		}
	}

	static function checkExpr(e:TypedExpr) {
		switch (e.expr) {
			case TCall({ expr: TField(_, FInstance(_, _, _.get() => {name: "assertEqualsConst"}))}, el):
				switch [el[0].expr, el[1].expr] {
					case [TConst(tc1), TConst(tc2)]:
						if (!constEquals(tc1, tc2)) {
							++numFailures;
							Context.warning('$tc2 should be $tc1', e.pos);
						}
					case [e1, e2]:
						++numFailures;
						Context.warning('$e2 should be $e1', e.pos);
				}
			case TCall({ expr: TField(_, FInstance(_, _, _.get() => {name: "assertEquals"}))}, el):
				for (e in el) {
					checkExpr(e);
				}
				switch (el[1].expr) {
					case (TConst(tc)):
						++numFailures;
						Context.warning('Unexpected constant $tc in assertEquals, use assertEqualsConst if this is intended', e.pos);
					case _:
				}
			case _:
				e.iter(checkExpr);
		}
	}

	static function constEquals(const1:TConstant, const2:TConstant) {
		return switch [const1, const2] {
			case [TInt(i1), TInt(i2)]: i1 == i2;
			case [TFloat(s1), TFloat(s2)]: s1 == s2;
			case [TString(s1), TString(s2)]: s1 == s2;
			case [TBool(b1), TBool(b2)]: b1 == b2;
			case [TNull, TNull]: true;
			case [TThis, TThis]: true;
			case [TSuper, TSuper]: true;
			case _: false;
		}
	}
	#end
}