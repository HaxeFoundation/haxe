package hxparse;

import haxe.macro.Context;
import haxe.macro.Expr;

using Lambda;
using haxe.macro.Tools;

/**
	The RuleBuilder interfaces provides syntactic shortcuts for writing lexer
	rules.
**/
#if !macro
@:autoBuild(hxparse.RuleBuilderImpl.build())
#end
interface RuleBuilder { }

class RuleBuilderImpl {
	macro static public function build():Array<Field> {
		var fields = Context.getBuildFields();
		var fieldExprs = new Map();
		var delays = [];
		var ret = [];
		var rules = [];
		for (field in fields) {
			if (field.access.exists(function(a) return a == AStatic))
				switch(field.kind) {
					case FVar(t, e) if (e != null):
						switch(e.expr) {
							case EMeta({name: ":rule"}, e):
								rules.push(field.name);
								delays.push(transformRule.bind(field, e, t, fieldExprs));
							case EMeta({name: ":mapping", params: args}, e):
								var offset = switch(args) {
									case [{expr: EConst(CInt(i))}]: Std.parseInt(i);
									case _: 0;
								}
								delays.push(transformMapping.bind(field, e, offset));
							case _:
								fieldExprs.set(field.name, e);
						}
					case _:
				}
			if (!field.meta.exists(function(m) return m.name == ":ruleHelper")) {
				ret.push(field);
			}
		}
		for (delay in delays)
			delay();
		var ruleIdents = [for (rv in rules) macro $i{rv}];
		ret.push( {
			name: "generatedRulesets",
			access: [APublic, AStatic],
			kind: FVar(TPath({
				name: "Array",
				pack: [],
				params: [TPType(TPath({
					name: "Ruleset",
					pack: ["hxparse"],
					params: [TPType(TPath( {
						name: "Dynamic",
						pack: []
					}))]
				}))]
			}), macro $a{ruleIdents}),
			pos: Context.currentPos()
		});
		return ret;
	}

	#if macro

	#if unifill

	static function handleUnicode(s:String, p:Position) {
		function getPosInfo(i, l) {
			var p = Context.getPosInfos(p);
			return Context.makePosition({
				min: p.min + i,
				max: p.min + i + l,
				file: p.file
			});
		}
		var uLength = unifill.Unifill.uLength(s);
		if (uLength == s.length) {
			return s;
		}
		var buf = new StringBuf();
		var itr = new unifill.InternalEncodingIter(s, 0, s.length);
		while (itr.hasNext()) {
			var i = itr.next();
			var c = unifill.InternalEncoding.charAt(s, i);
			switch (c) {
				case '[':
					buf.add("(");
					var first = true;
					while(true) {
						if (!itr.hasNext()) {
							Context.error("Unterminated regular expression", getPosInfo(itr.index, 1));
						}
						var i = itr.next();
						var c = unifill.InternalEncoding.charAt(s, i);
						switch (c) {
							case "]":
								break;
							case "^" if (first):
								var p = unifill.InternalEncoding.codePointCount(s, 0, i);
								Context.error("Not-ranges are not supported in unicode strings", getPosInfo(i, 1));
							case _:
								if (!first) {
									buf.add("|");
								}
								buf.add("(");
								if (!itr.hasNext()) {
									Context.error("Unterminated regular expression", getPosInfo(itr.index, 1));
								}
								var w = unifill.InternalEncoding.codePointWidthAt(s, i);
								if (unifill.InternalEncoding.charAt(s, i + w) == "-") {
									itr.next();
									if (!itr.hasNext()) {
										Context.error("Unterminated regular expression", getPosInfo(itr.index, 1));
									}
									var k = itr.next();
									var cNext = unifill.InternalEncoding.charAt(s, k);
									if (unifill.InternalEncoding.codePointAt(c, 0) > 0x7F) {
										Context.error("Unicode ranges are not supported", getPosInfo(i, 3));
									} else {
										buf.add("[");
										buf.add(c);
										buf.add("-");
										buf.add(cNext);
										buf.add("]");
									}
								} else {
									buf.add(c);
								}
								buf.add(")");
						}
						first = false;
					}
					buf.add(")");
				case _:
					buf.add(c);
			}
		}
		return buf.toString();
	}

	#end

	static function makeRule(fields:Map<String,Expr>, rule:Expr):String {
		return switch(rule) {
			case macro $v{(s:String)}: #if unifill handleUnicode(s, rule.pos) #else s #end;
			case macro $i{i}: makeRule(fields, fields.get(i));
			case macro $e1 + $e2: "(" + makeRule(fields, e1) +")(" + makeRule(fields, e2) +")";
			case {expr:EConst(CRegexp(r, opt))}:
				if (opt != "") {
					Context.error("Cannot use regular expression flags for lexer rules", rule.pos);
				}
				r;
			case _: Context.error("Invalid rule", rule.pos);
		}
	}

	static function transformRule(field:Field, e:Expr, t:ComplexType, fields:Map<String,Expr>) {
		var el = switch(e.expr) {
			case EArrayDecl(el): el;
			case _: Context.error("Expected pattern => function map declaration", e.pos);
		}
		var el = el.map(function(e) {
			function loop(e:Expr) {
				return switch(e.expr) {
					case EBinop(OpArrow, rule, e):
						macro @:pos(e.pos) {rule:$v{makeRule(fields, rule)}, func:function(lexer:hxparse.Lexer):$t return $e};
					case EConst(CIdent(s)) if (fields.exists(s)):
						loop(fields.get(s));
					case _:
						Context.error("Expected pattern => function", e.pos);
				}
			}
			return loop(e);
		});
		var e = macro $a{el};
		var e = macro hxparse.Lexer.buildRuleset($e, $v{field.name});
		field.kind = FVar(null, e);
		return e;
	}

	static function transformMapping(field:Field, e:Expr, offset:Int) {
		var t = Context.typeof(e).follow();
		var sl = [];
		switch(t) {
			case TAnonymous(a):
				for (f in a.get().fields) {
					var name = macro @:pos(e.pos) $i{f.name};
					var cName = f.name.charAt(offset).toLowerCase() + f.name.substr(offset + 1);
					sl.push(macro $v{cName} => $name);
				}
			case _:
				Context.error("Invalid mapping type", e.pos);
		}
		var e = macro $a{sl};
		field.kind = FVar(null, e);
		return e;
	}

	#end
}