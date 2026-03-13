package hxparse;

import haxe.macro.Context;
import haxe.macro.Expr;

using haxe.macro.Tools;
using Lambda;

private typedef ParserCase = {
	expr: Expr,
	head: Expr,
	tail: Array<Expr>
}

private enum CaseGroup {
	Simple(group:Array<ParserCase>);
	Complex(c:ParserCase);
}

class ParserBuilderImpl {
	static public function build():Array<Field> {
		var fields = Context.getBuildFields();
		for (field in fields) {
			switch(field.kind) {
				case FFun(fun) if (fun.expr != null):
					fun.expr = map(fun.expr);
				case _:
			}
		}
		return fields;
	}

	static function punion(p1:Position, p2:Position) {
		var p1 = Context.getPosInfos(p1);
		var p2 = Context.getPosInfos(p2);
		return Context.makePosition({
			file: p1.file,
			min: p1.min < p2.min ? p1.min : p2.min,
			max: p1.max > p2.max ? p1.max : p2.max
		});
	}

	static function map(e:Expr) {
		return switch(e.expr) {
			case ESwitch({expr: EConst(CIdent("stream"))}, cl, edef):
				transformSwitch(cl, edef);
			case EBlock([]):
				e;
			case EBlock(el):
				var elast = el.pop();
				var el = el.map(map);
				el.push(map(elast));
				macro @:pos(e.pos) $b{el};
			case _: e.map(map);
		}
	}

	static function transformSwitch(cl:Array<Case>, edef:Null<Expr>) {
		if (edef != null)
			cl.push({values: [macro _], expr: edef, guard: null});
		return transformCases(cl);
	}

	static function transformCases(cl:Array<Case>) {
		var groups = [];
		var group = [];
		var def = noMatch;
		for (c in cl) {
			switch(c.values) {
				case [{expr:EArrayDecl(el)}]:
					var head = el.shift();
					var chead = {head:head, tail: el, expr:c.expr == null ? macro null : map(c.expr)};
					switch(head.expr) {
						case EBinop(_):
							if (group.length > 0) groups.push(Simple(group));
							groups.push(Complex(chead));
							group = [];
						case _:
							group.push(chead);
					}
				case [{expr:EConst(CIdent("_"))}]:
					def = c.expr == null ? macro null : map(c.expr);
				case [e]:
					Context.error("Expected [ patterns ]", e.pos);
				case _:
					Context.error("Comma notation is not allowed while matching streams", punion(c.values[0].pos, c.values[c.values.length - 1].pos));
			}
		}
		if (group.length > 0)
			groups.push(Simple(group));

		var last = groups.pop();
		var elast = makeCase(last,def);
		while (groups.length > 0) {
			elast = makeCase(groups.pop(), elast);
		}
		return elast;
	}

	static var unexpected = macro unexpected();
	static var noMatch = macro throw noMatch();

	static function makeCase(g:CaseGroup, def:Expr) {
		return switch(g) {
			case Simple(group):
				var cl = group.map(makeInner);
				cl.iter(function(c) {
					c.expr = macro @:pos(c.expr.pos) { junk(); ${c.expr}; };
				});
				{
					pos: def.pos,
					expr: ESwitch(macro peek(0), cl, def)
				}
			case Complex(c):
				var inner = makeInner(c);
				makePattern(c.head, inner.expr, def);
		}
	}

	static function makeInner(c:ParserCase) {
		var last = c.tail.pop();
		if (last == null) {
			return {values:[c.head], guard:null, expr: c.expr};
		}
		var elast = makePattern(last, c.expr, unexpected);
		while (c.tail.length > 0)
			elast = makePattern(c.tail.pop(), elast, unexpected);
		return {values: [c.head], guard: null, expr: elast};
	}

	static function makePattern(pat:Expr, e:Expr, def:Expr) {
		return switch(pat.expr) {
			case EBinop(OpAssign, {expr: EConst(CIdent(s))}, e2):
				if (def == unexpected || def == noMatch) {
					var e1 = s == "_" ? e2 : macro var $s = $e2;
					macro {
						$e1;
						$e;
					}
				} else {
					buildExtractor(pat, e, e2, s, def);
				}
			case EBinop(OpBoolAnd, e1, e2):
				macro @:pos(pat.pos) {
					switch peek(0) {
						case $e1 if ($e2):
							junk();
							$e;
						case _: $def;
					}
				}
			case EBinop(OpBoolOr, e1, e2):
				makePattern(e1, e, macro throw stream.curPos() + ": " +$e2);
			case _:
				macro @:pos(pat.pos) switch peek(0) {
					case $pat:
						junk();
						$e;
					case _: $def;
				}
		}
	}

	static function buildExtractor(pat, e, e2, s, def) {
		var e1 = s == "_" ? e2 : macro var $s = $e2;
		return macro @:pos(pat.pos) {
			try {
				$e1;
				$e;
			} catch (_:hxparse.NoMatch<Dynamic>) {
				$def;
			}
		}
	}
}