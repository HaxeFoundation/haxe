/*
 * Copyright (C)2005-2013 Haxe Foundation
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

class Printer {
	var tabs:String;

	public function new() {
		tabs = "";
	}
	
	public function printUnop(op:Unop) return switch(op) {
		case OpIncrement: "++";
		case OpDecrement: "--";
		case OpNot: "!";
		case OpNeg: "-";
		case OpNegBits: "~";
	}
	
	public function printBinop(op:Binop) return switch(op) {
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
		case OpAssignOp(op):
			printBinop(op)
			+ "=";
	}
		
	public function printConstant(c:Constant) return switch(c) {
		case CString(s): '"$s"';
		case CIdent(s),
			#if !haxe3
			CType(s),
			#end
			CInt(s),
			CFloat(s):
				s;
		case CRegexp(s,opt): '~/$s/$opt';
	}
	
	public function printTypeParam(param:TypeParam) return switch(param) {
		case TPType(ct): printComplexType(ct);
		case TPExpr(e): printExpr(e);
	}
	
	public function printTypePath(tp:TypePath) return
		(tp.pack.length > 0 ? tp.pack.join(".") + "." : "")
		+ tp.name
		+ (tp.sub != null ? '.$tp.sub' : "")
		+ (tp.params.length > 0 ? "<" + tp.params.map(printTypeParam).array().join(",") + ">" : "")
	
	// TODO: check if this can cause loops
	public function printComplexType(ct:ComplexType) return switch(ct) {
		case TPath(tp): printTypePath(tp);
		case TFunction(args, ret): args.map(printComplexType).array().join("->") + "->" + printComplexType(ret);
		case TAnonymous(fields): "{" + fields.map(printField).array().join(",") + "}";
		case TParent(ct): "(" + printComplexType(ct) + ")";
		case TOptional(ct): "?" + printComplexType(ct);
		case TExtend(tp, fields): '{${printTypePath(tp)} >, ${fields.map(printField).array().join(",")}}';
	}
	
	public function printMetadata(meta:MetadataEntry) return
		'@${meta.name}'
		+ (meta.params.length > 0 ? '(${printExprs(meta.params,",")})' : "")

	public function printField(field:Field) return
		(field.meta != null && field.meta.length > 0 ? field.meta.map(printMetadata).array().join(" ") : "")
		+ switch(field.kind) {
			case FVar(t, eo): 'var $field.name:${printComplexType(t)}' + opt(eo, printExpr, "=");
			case FProp(get, set, to, eo): 'var $field.name($get,$set)';
			case FFun(func): "function " + printFunction(func);
		}
	
	public function printTypeParamDecl(tpd:TypeParamDecl) return
		tpd.name
		+ (tpd.params.length > 0 ? "<" + tpd.params.map(printTypeParamDecl).array().join(",") + ">" : "")
		+ (tpd.constraints.length > 0 ? ":(" + tpd.constraints.map(printComplexType).array().join(",") + ")" : "")
	
	public function printFunctionArg(arg:FunctionArg) return
		(arg.opt ? "?" : "")
		+ arg.name
		+ opt(arg.type, printComplexType, ":")
		+ opt(arg.value, printExpr, "=")
	
	public function printFunction(func:Function) return
		(func.params.length > 0 ? "<" + func.params.map(printTypeParamDecl).array().join(",") + ">" : "")
		+ "(" + func.args.map(printFunctionArg).array().join(",") + ")"
		+ opt(func.ret, printComplexType, ":")
		+ opt(func.expr, printExpr, " ")
	
	public function printVar(v:Var) return
		v.name
		+ opt(v.type, printComplexType, ":")
		+ opt(v.expr, printExpr, "=")
	
	
	public function printExpr(e:Expr) return switch(e.expr) {
		case EConst(c): printConstant(c);
		case EArray(e1, e2): '${printExpr(e1)}[${printExpr(e2)}]';
		case EBinop(op, e1, e2): '${printExpr(e1)}${printBinop(op)}${printExpr(e2)}';
		case EField(e1, n)
			#if !haxe3
			,EType(e1, n)
			#end : '${printExpr(e1)}.$n';
		case EParenthesis(e1): '(${printExpr(e1)})';
		case EObjectDecl(fl):
			"{" + fl.map(function(fld) return '${fld.field}:${printExpr(fld.expr)}').array().join(",") + "}";
		case EArrayDecl(el): '[${printExprs(el, ",")}]';
		case ECall(e1, el): '${printExpr(e1)}(${printExprs(el,",")})';
		case ENew(tp, el): 'new ${printTypePath(tp)}(${printExprs(el,",")})';
		case EUnop(op, true, e1): printExpr(e1) + printUnop(op); 
		case EUnop(op, false, e1): printUnop(op) + printExpr(e1);
		case EFunction(no, func) if (no != null): 'function $no' + printFunction(func);
		case EFunction(_, func): "function " +printFunction(func);
		case EVars(vl): "var " +vl.map(printVar).array().join(",");
		case EBlock([]): '{\n$tabs}';
		case EBlock(el):
			var old = tabs;
			tabs += "\t";
			var s = '{\n$tabs' + printExprs(el, ';\n$tabs');
			tabs = old;
			s + ';\n$tabs}';
		case EFor(e1, e2): 'for(${printExpr(e1)}) ${printExpr(e2)}';
		case EIn(e1, e2): '${printExpr(e1)} in ${printExpr(e2)}';
		case EIf(econd, eif, eelse): 'if(${printExpr(econd)}) ${printExpr(eif)} ${opt(eelse,printExpr,"else ")}';
		case EWhile(econd, e1, true): 'while(${printExpr(econd)}) ${printExpr(e1)}';
		case EWhile(econd, e1, false): 'do ${printExpr(e1)} while(${printExpr(econd)})';
		// TODO: remove the implicit case block
		case ESwitch(e1, cl, edef):
			var old = tabs;
			tabs += "\t";
			var s = 'switch ${printExpr(e1)} {\n$tabs' +
				cl.map(function(c)
					return 'case ${printExprs(c.values, ",")}'
						+ (c.guard != null ? 'if(${printExpr(c.guard)}):' : ":")
						+ (opt(c.expr, printExpr)))
				.array().join(';\n$tabs');
			if (edef != null)
				s += ';\n${tabs}default:${printExpr(edef)}';
			tabs = old;
			s + ';\n$tabs}';
		case ETry(e1, cl):
			'try ${printExpr(e1)}'
			+ cl.map(function(c) return ' catch(${c.name}:${printComplexType(c.type)}) ${printExpr(c.expr)}').array().join("");
		case EReturn(eo): "return" + opt(eo, printExpr, " ");
		case EBreak: "break";
		case EContinue: "continue";
		case EUntyped(e1): "untyped " +printExpr(e1);
		case EThrow(e1): "throw " +printExpr(e1);
		case ECast(e1, cto) if (cto != null): 'cast(${printExpr(e1)}, ${printComplexType(cto)})';
		case ECast(e1, _): "cast " +printExpr(e1);
		case EDisplay(e1, _): '#DISPLAY(${printExpr(e1)})';
		case EDisplayNew(tp): '#DISPLAY(${printTypePath(tp)})';
		case ETernary(econd, eif, eelse): '${printExpr(econd)} ? ${printExpr(eif)} : $ {printExpr(eelse) }';
		case ECheckType(e1, ct): '#CHECK_TYPE(${printExpr(e1)}, ${printComplexType(ct)})';
		case EMeta(meta, e1): printMetadata(meta) + " " +printExpr(e1);
	}
	
	public function printExprs(el:Array<Expr>, sep:String) {
		return el.map(printExpr).array().join(sep);
	}
	
	function opt<T>(v:T, f:T->String, prefix = "") return v == null ? "" : (prefix + f(v))
}