/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package haxe.macro;

import haxe.macro.Expr;

using Lambda;
using StringTools;

/**
	This class provides some utility methods to convert elements from the
	macro context to a human-readable String representation.
 */
class Printer {
	var tabs:String;
	var tabString:String;

	public function new(?tabString = "\t") {
		tabs = "";
		this.tabString = tabString;
	}

	public function printUnop(op:Unop)
		return switch (op) {
			case OpIncrement: "++";
			case OpDecrement: "--";
			case OpNot: "!";
			case OpNeg: "-";
			case OpNegBits: "~";
			case OpSpread: "...";
		}

	public function printBinop(op:Binop)
		return switch (op) {
			case OpAdd: "+";
			case OpMult: "*";
			case OpDiv: "/";
			case OpSub: "-";
			case OpAssign: "=";
			case OpEq: "==";
			case OpNotEq: "!=";
			case OpGt: ">";
			case OpGte: ">=";
			case OpLt: "<";
			case OpLte: "<=";
			case OpAnd: "&";
			case OpOr: "|";
			case OpXor: "^";
			case OpBoolAnd: "&&";
			case OpBoolOr: "||";
			case OpShl: "<<";
			case OpShr: ">>";
			case OpUShr: ">>>";
			case OpMod: "%";
			case OpInterval: "...";
			case OpArrow: "=>";
			case OpIn: "in";
			case OpNullCoal: "??";
			case OpAssignOp(op):
				printBinop(op) + "=";
		}

	function escapeString(s:String, delim:String) {
		return delim
			+ s.replace('\\', '\\\\')
				.replace("\n", "\\n")
				.replace("\t", "\\t")
				.replace("\r", "\\r")
				.replace("'", "\\'")
				.replace('"', "\\\"") #if sys .replace("\x00", "\\x00") #end + delim;
	}

	public function printFormatString(s:String) {
		return escapeString(s, "'");
	}

	public function printString(s:String) {
		return escapeString(s, '"');
	}

	public function printConstant(c:Constant)
		return switch (c) {
			case CString(s, SingleQuotes): printFormatString(s);
			case CString(s, _): printString(s);
			case CIdent(s), CInt(s, null), CFloat(s, null):
				s;
			case CInt(s, suffix), CFloat(s, suffix):
				s + suffix;
			case CRegexp(s, opt): '~/$s/$opt';
		}

	public function printTypeParam(param:TypeParam)
		return switch (param) {
			case TPType(ct): printComplexType(ct);
			case TPExpr(e): printExpr(e);
		}

	public function printTypePath(tp:TypePath)
		return (tp.pack.length > 0 ? tp.pack.join(".") + "." : "")
			+ tp.name
			+ (tp.sub != null ? '.${tp.sub}' : "")
			+ (tp.params == null ? "" : tp.params.length > 0 ? "<" + tp.params.map(printTypeParam).join(", ") + ">" : "");

	// TODO: check if this can cause loops
	public function printComplexType(ct:ComplexType)
		return switch (ct) {
			case TPath(tp): printTypePath(tp);
			case TFunction(args, ret):
				var wrapArgumentsInParentheses = switch args {
					// type `:(a:X) -> Y` has args as [TParent(TNamed(...))], i.e `a:X` gets wrapped in `TParent()`. We don't add parentheses to avoid printing `:((a:X)) -> Y`
					case [TParent(t)]: false;
					// this case catches a single argument that's a type-path, so that `X -> Y` prints `X -> Y` not `(X) -> Y`
					case [TPath(_) | TOptional(TPath(_))]: false;
					default: true;
				}
				var argStr = args.map(printComplexType).join(", ");
				(wrapArgumentsInParentheses ? '($argStr)' : argStr) + " -> " + (switch ret {
					// wrap return type in parentheses if it's also a function
					case TFunction(_): '(${printComplexType(ret)})';
					default: (printComplexType(ret) : String);
				});
			case TAnonymous(fields): "{ " + [for (f in fields) printField(f) + "; "].join("") + "}";
			case TParent(ct): "(" + printComplexType(ct) + ")";
			case TOptional(ct): "?" + printComplexType(ct);
			case TNamed(n, ct): n + ":" + printComplexType(ct);
			case TExtend(tpl, fields):
				var types = [for (t in tpl) "> " + printTypePath(t) + ", "].join("");
				var fields = [for (f in fields) printField(f) + "; "].join("");
				'{${types}${fields}}';
			case TIntersection(tl): tl.map(printComplexType).join(" & ");
		}

	public function printMetadata(meta:MetadataEntry)
		return '@${meta.name}' + ((meta.params != null && meta.params.length > 0) ? '(${printExprs(meta.params, ", ")})' : "");

	public function printAccess(access:Access)
		return switch (access) {
			case AStatic: "static";
			case APublic: "public";
			case APrivate: "private";
			case AOverride: "override";
			case AInline: "inline";
			case ADynamic: "dynamic";
			case AMacro: "macro";
			case AFinal: "final";
			case AExtern: "extern";
			case AAbstract: "abstract";
			case AOverload: "overload";
			case AEnum: "enum";
		}

	public function printField(field:Field) {
		inline function orderAccess(access:Array<Access>) {
			// final should always be printed last
			// (does not modify input array)
			return access.has(AFinal) ? access.filter(a -> !a.match(AFinal)).concat([AFinal]) : access;
		}
		return (field.doc != null
			&& field.doc != "" ? "/**\n"
				+ tabs
				+ tabString
				+ StringTools.replace(field.doc, "\n", "\n" + tabs + tabString)
				+ "\n"
				+ tabs
				+ "**/\n"
				+ tabs : "")
			+ (field.meta != null && field.meta.length > 0 ? field.meta.map(printMetadata).join('\n$tabs') + '\n$tabs' : "")
			+ (field.access != null && field.access.length > 0 ? orderAccess(field.access).map(printAccess).join(" ") + " " : "")
			+ switch (field.kind) {
				case FVar(t,
					eo): ((field.access != null && field.access.has(AFinal)) ? '' : 'var ')
						+ '${field.name}'
						+ opt(t, printComplexType, " : ")
						+ opt(eo, printExpr, " = ");
				case FProp(get, set, t, eo): 'var ${field.name}($get, $set)' + opt(t, printComplexType, " : ") + opt(eo, printExpr, " = ");
				case FFun(func): 'function ${field.name}' + printFunction(func);
			}}

	public function printTypeParamDecl(tpd:TypeParamDecl)
		return (tpd.meta != null && tpd.meta.length > 0 ? tpd.meta.map(printMetadata).join(" ") + " " : "")
			+ tpd.name
			+ (tpd.params != null && tpd.params.length > 0 ? "<" + tpd.params.map(printTypeParamDecl).join(", ") + ">" : "")
			+ (tpd.constraints != null && tpd.constraints.length > 0 ? ":(" + tpd.constraints.map(printComplexType).join(" & ") + ")" : "")
			+ (tpd.defaultType != null ? "=" + printComplexType(tpd.defaultType) : "");

	public function printFunctionArg(arg:FunctionArg)
		return (arg.opt ? "?" : "") + arg.name + opt(arg.type, printComplexType, ":") + opt(arg.value, printExpr, " = ");

	public function printFunction(func:Function, ?kind:FunctionKind) {
		var skipParentheses = switch func.args {
			case [{type: null}]: kind == FArrow;
			case _: false;
		}
		return (func.params == null ? "" : func.params.length > 0 ? "<" + func.params.map(printTypeParamDecl).join(", ") + ">" : "")
			+ (skipParentheses ? "" : "(")
			+ func.args.map(printFunctionArg).join(", ")
			+ (skipParentheses ? "" : ")")
			+ (kind == FArrow ? " ->" : "")
			+ opt(func.ret, printComplexType, ":")
			+ opt(func.expr, printExpr, " ");
	}

	public function printVar(v:Var) {
		var s = v.name + opt(v.type, printComplexType, ":") + opt(v.expr, printExpr, " = ");
		return switch v.meta {
			case null | []: s;
			case meta: meta.map(printMetadata).join(" ") + " " + s;
		}
	}

	public function printObjectFieldKey(of:ObjectField) {
		return switch (of.quotes) {
			case null | Unquoted: of.field;
			case Quoted: '"${of.field}"'; // TODO: Have to escape that?
		}
	}

	public function printObjectField(of:ObjectField) {
		return '${printObjectFieldKey(of)} : ${printExpr(of.expr)}';
	}

	public function printExpr(e:Expr)
		return e == null ? "#NULL" : switch (e.expr) {
			case EConst(c): printConstant(c);
			case EArray(e1, e2): '${printExpr(e1)}[${printExpr(e2)}]';
			case EBinop(op, e1, e2): '${printExpr(e1)} ${printBinop(op)} ${printExpr(e2)}';
			case EField(e1, n, kind): kind == Safe ? '${printExpr(e1)}?.$n' : '${printExpr(e1)}.$n';
			case EParenthesis(e1): '(${printExpr(e1)})';
			case EObjectDecl(fl):
				"{ " + fl.map(function(fld) return printObjectField(fld)).join(", ") + " }";
			case EArrayDecl(el): '[${printExprs(el, ", ")}]';
			case ECall(e1, el): '${printExpr(e1)}(${printExprs(el, ", ")})';
			case ENew(tp, el): 'new ${printTypePath(tp)}(${printExprs(el, ", ")})';
			case EUnop(op, true, e1): printExpr(e1) + printUnop(op);
			case EUnop(op, false, e1): printUnop(op) + printExpr(e1);
			case EFunction(FNamed(no, inlined), func): (inlined ? 'inline ' : '') + 'function $no' + printFunction(func);
			case EFunction(kind, func): (kind != FArrow ? "function" : "") + printFunction(func, kind);
			case EVars([]): "var ";
			case EVars(vl): ((vl[0].isStatic) ? "static " : "") + ((vl[0].isFinal) ? "final " : "var ") + vl.map(printVar).join(", ");
			case EBlock([]): '{ }';
			case EBlock(el):
				var old = tabs;
				tabs += tabString;
				var s = '{\n$tabs' + printExprs(el, ';\n$tabs');
				tabs = old;
				s + ';\n$tabs}';
			case EFor(e1, e2): 'for (${printExpr(e1)}) ${printExpr(e2)}';
			case EIf(econd, eif, null): 'if (${printExpr(econd)}) ${printExpr(eif)}';
			case EIf(econd, eif, eelse): 'if (${printExpr(econd)}) ${printExpr(eif)} else ${printExpr(eelse)}';
			case EWhile(econd, e1, true): 'while (${printExpr(econd)}) ${printExpr(e1)}';
			case EWhile(econd, e1, false): 'do ${printExpr(e1)} while (${printExpr(econd)})';
			case ESwitch(e1, cl, edef):
				var old = tabs;
				tabs += tabString;
				var s = 'switch ${printExpr(e1)} {\n$tabs'
					+ cl.map(function(c) return 'case ${printExprs(c.values, ", ")}' + (c.guard != null ? ' if (${printExpr(c.guard)}):' : ":")
						+ (c.expr != null ? (opt(c.expr, printExpr)) + ";" : ""))
						.join('\n$tabs');
				if (edef != null)
					s += '\n${tabs}default:' + (edef.expr == null ? "" : printExpr(edef) + ";");
				tabs = old;
				s + '\n$tabs}';
			case ETry(e1, cl):
				'try ${printExpr(e1)}' + cl.map(function(c) return
					' catch(${c.name}${c.type == null ? '' : (':' + printComplexType(c.type))}) ${printExpr(c.expr)}')
					.join("");
			case EReturn(eo): "return" + opt(eo, printExpr, " ");
			case EBreak: "break";
			case EContinue: "continue";
			case EUntyped(e1): "untyped " + printExpr(e1);
			case EThrow(e1): "throw " + printExpr(e1);
			case ECast(e1, cto) if (cto != null): 'cast(${printExpr(e1)}, ${printComplexType(cto)})';
			case ECast(e1, _): "cast " + printExpr(e1);
			case EIs(e1, ct): '${printExpr(e1)} is ${printComplexType(ct)}';
			case EDisplay(e1, _): '#DISPLAY(${printExpr(e1)})';
			case ETernary(econd, eif, eelse): '${printExpr(econd)} ? ${printExpr(eif)} : ${printExpr(eelse)}';
			case ECheckType(e1, ct): '(${printExpr(e1)} : ${printComplexType(ct)})';
			case EMeta({name: ":implicitReturn"}, {expr: EReturn(e1)}): printExpr(e1);
			case EMeta(meta, e1): printMetadata(meta) + " " + printExpr(e1);
		}

	public function printExprs(el:Array<Expr>, sep:String) {
		return el.map(printExpr).join(sep);
	}

	function printExtension(tpl:Array<TypePath>, fields:Array<Field>) {
		return '{\n$tabs>'
			+ tpl.map(printTypePath).join(',\n$tabs>')
			+ ","
			+ (fields.length > 0 ? ('\n$tabs' + fields.map(printField).join(';\n$tabs') + ";\n}") : ("\n}"));
	}

	function printStructure(fields:Array<Field>) {
		return fields.length == 0 ? "{ }" : '{\n$tabs' + fields.map(printField).join(';\n$tabs') + ";\n}";
	}

	public function printTypeDefinition(t:TypeDefinition, printPackage = true):String {
		var old = tabs;
		tabs = tabString;

		var str = t == null ? "#NULL" : (printPackage && t.pack.length > 0 && t.pack[0] != "" ? "package " + t.pack.join(".") + ";\n" : "")
			+ (t.doc != null && t.doc != "" ? "/**\n" + tabString + StringTools.replace(t.doc, "\n", "\n" + tabString) + "\n**/\n" : "")
			+ (t.meta != null && t.meta.length > 0 ? t.meta.map(printMetadata).join(" ") + " " : "")
			+ (t.isExtern ? "extern " : "")
			+ switch (t.kind) {
				case TDEnum:
					"enum "
					+ t.name
					+ ((t.params != null && t.params.length > 0) ? "<" + t.params.map(printTypeParamDecl).join(", ") + ">" : "")
					+ " {\n"
					+ [
						for (field in t.fields)
							tabs
							+ (field.doc != null
								&& field.doc != "" ? "/**\n"
									+ tabs
									+ tabString
									+ StringTools.replace(field.doc, "\n", "\n" + tabs + tabString)
									+ "\n"
									+ tabs
									+ "**/\n"
									+ tabs : "")
							+ (field.meta != null && field.meta.length > 0 ? field.meta.map(printMetadata).join(" ") + " " : "")
							+ (switch (field.kind) {
								case FVar(t, _): field.name + opt(t, printComplexType, ":");
								case FProp(_, _, _, _): throw "FProp is invalid for TDEnum.";
								case FFun(func): field.name + printFunction(func);
							})
							+ ";"].join("\n") + "\n}";
				case TDStructure:
					"typedef "
					+ t.name
					+ ((t.params != null && t.params.length > 0) ? "<" + t.params.map(printTypeParamDecl).join(", ") + ">" : "")
					+ " = {\n"
					+ [
						for (f in t.fields) {
							tabs + printField(f) + ";";
						}
					].join("\n") + "\n}";
				case TDClass(superClass, interfaces, isInterface, isFinal, isAbstract):
					(isFinal ? "final " : "")
						+ (isAbstract ? "abstract " : "")
						+ (isInterface ? "interface " : "class ")
						+ t.name
						+ (t.params != null && t.params.length > 0 ? "<" + t.params.map(printTypeParamDecl).join(", ") + ">" : "")
						+ (superClass != null ? " extends " + printTypePath(superClass) : "")
						+ (interfaces != null ? (isInterface ? [for (tp in interfaces) " extends " + printTypePath(tp)] : [
							for (tp in interfaces)
								" implements " + printTypePath(tp)
						]).join("") : "")
						+ " {\n"
						+ [
							for (f in t.fields) {
								tabs + printFieldWithDelimiter(f);
							}
						].join("\n") + "\n}";
				case TDAlias(ct):
					"typedef "
					+ t.name
					+ ((t.params != null && t.params.length > 0) ? "<" + t.params.map(printTypeParamDecl).join(", ") + ">" : "")
					+ " = "
					+ (switch (ct) {
						case TExtend(tpl, fields): printExtension(tpl, fields);
						case TAnonymous(fields): printStructure(fields);
						case _: printComplexType(ct);
					})
					+ ";";
				case TDAbstract(tthis, tflags, from, to):
					var from = from == null ? [] : from.copy();
					var to = to == null ? [] : to.copy();
					var isEnum = false;

					if (tflags != null) {
						for (flag in tflags) {
							switch (flag) {
								case AbEnum: isEnum = true;
								case AbFrom(ct): from.push(ct);
								case AbTo(ct): to.push(ct);
							}
						}
					}

					(isEnum ? "enum " : "")
					+ "abstract "
					+ t.name
					+ ((t.params != null && t.params.length > 0) ? "<" + t.params.map(printTypeParamDecl).join(", ") + ">" : "")
					+ (tthis == null ? "" : "(" + printComplexType(tthis) + ")")
					+ [for (f in from) " from " + printComplexType(f)].join("")
					+ [for (f in to) " to " + printComplexType(f)].join("")
					+ " {\n"
					+ [
						for (f in t.fields) {
							tabs + printFieldWithDelimiter(f);
						}
					].join("\n") + "\n}";
				case TDField(kind, access):
					tabs = old;
					(access != null && access.length > 0 ? access.map(printAccess).join(" ") + " " : "") + switch (kind) {
						case FVar(type,
							eo): ((access != null && access.has(AFinal)) ? '' : 'var ') + '${t.name}' + opt(type, printComplexType, " : ")
								+ opt(eo, printExpr, " = ") + ";";
						case FProp(get, set, type, eo): 'var ${t.name}($get, $set)'
							+ opt(type, printComplexType, " : ")
							+ opt(eo, printExpr, " = ")
							+ ";";
						case FFun(func): 'function ${t.name}' + printFunction(func) + switch func.expr {
								case {expr: EBlock(_)}: "";
								case _: ";";
							};
					}
			} tabs = old;

		return str;
	}

	function printFieldWithDelimiter(f:Field):String {
		return printField(f) + switch (f.kind) {
			case FVar(_, _), FProp(_, _, _, _): ";";
			case FFun({expr: null}): ";";
			case FFun({expr: {expr: EBlock(_)}}): "";
			case FFun(_): ";";
			case _: "";
		};
	}

	function opt<T>(v:T, f:T->String, prefix = "")
		return v == null ? "" : (prefix + f(v));

	public function printExprWithPositions(e:Expr) {
		var buffer = new StringBuf();
		function format4(i:Int) {
			return StringTools.lpad(Std.string(i), " ", 4);
		}
		function loop(tabs:String, e:Expr) {
			function add(s:String, ?p = null) {
				if (p == null) {
					p = e.pos;
				}
				var p = #if macro haxe.macro.Context.getPosInfos(p) #else e.pos #end;
				buffer.add('${format4(p.min)}-${format4(p.max)} $tabs$s\n');
			}
			function loopI(e:Expr)
				loop(tabs + tabString, e);
			switch (e.expr) {
				case EConst(c):
					add(printConstant(c));
				case EArray(e1, e2):
					add("EArray");
					loopI(e1);
					loopI(e2);
				case EBinop(op, e1, e2):
					add("EBinop " + printBinop(op));
					loopI(e1);
					loopI(e2);
				case EField(e, field, kind):
					if (kind == null) kind = Normal;
					add('EField $field (${kind.getName()})');
					loopI(e);
				case EParenthesis(e):
					add("EParenthesis");
					loopI(e);
				case EObjectDecl(fields):
					add("EObjectDecl");
					for (field in fields) {
						add(field.field); // TODO: we don't have the field pos?
						loopI(field.expr);
					}
				case EArrayDecl(values):
					add("EArrayDecl");
					values.iter(loopI);
				case ECall(e, params):
					add("ECall");
					loopI(e);
					params.iter(loopI);
				case ENew(tp, params):
					add("ENew " + printTypePath(tp));
					params.iter(loopI);
				case EUnop(op, postFix, e):
					add("EUnop " + printUnop(op));
					loopI(e);
				case EVars(vars):
					add("EVars");
					for (v in vars) {
						if (v.expr != null) {
							add(v.name);
							loopI(v.expr);
						}
					}
				case EFunction(_, f):
					add("EFunction");
					if (f.expr != null) {
						loopI(f.expr);
					}
				case EBlock(exprs):
					add("EBlock");
					exprs.iter(loopI);
				case EFor(it, expr):
					add("EFor");
					loopI(it);
					loopI(expr);
				case EIf(econd, eif, eelse):
					add("EIf");
					loopI(econd);
					loopI(eif);
					if (eelse != null) {
						loopI(eelse);
					}
				case EWhile(econd, e, normalWhile):
					add("EWhile");
					loopI(econd);
					loopI(e);
				case ESwitch(e, cases, edef):
					add("ESwitch");
					loopI(e);
					for (c in cases) {
						for (pat in c.values) {
							loop(tabs + tabString + tabString, pat);
						}
						if (c.expr != null) {
							loop(tabs + tabString + tabString + tabString, c.expr);
						}
					}
					if (edef != null) {
						loop(tabs + tabString + tabString + tabString, edef);
					}
				case ETry(e, catches):
					add("ETry");
					loopI(e);
					for (c in catches) {
						loop(tabs + tabString + tabString, c.expr);
					}
				case EReturn(e):
					add("EReturn");
					if (e != null) {
						loopI(e);
					}
				case EBreak:
					add("EBreak");
				case EContinue:
					add("EContinue");
				case EUntyped(e):
					add("EUntyped");
					loopI(e);
				case EThrow(e):
					add("EThrow");
					loopI(e);
				case ECast(e, t):
					add("ECast");
					loopI(e);
				case EIs(e, t):
					add("EIs");
					loopI(e);
				case EDisplay(e, displayKind):
					add("EDisplay");
					loopI(e);
				case ETernary(econd, eif, eelse):
					add("ETernary");
					loopI(econd);
					loopI(eif);
					loopI(eelse);
				case ECheckType(e, t):
					add("ECheckType");
					loopI(e);
				case EMeta(s, e):
					add("EMeta " + printMetadata(s));
					loopI(e);
			}
		}
		loop("", e);
		return buffer.toString();
	}
}
