package cases;

using StringTools;

/**
	A workload shaped like `hxsl.Macros.buildShader`: tokenize and parse a
	small shader language into an enum tree, type check it with scoped
	environments, then link the shaders (collect the used variables, rename,
	print).
**/
class Shader {
	public static function run() {
		var sum = 0;
		for (k in 0...4) {
			var shaders = [for (i in 0...12) parse(generate(i + k * 12, 40))];
			var checked = [for (s in shaders) Checker.check(s)];
			var out = Linker.link(checked);
			sum = (sum + out.length + Checker.hash(out)) & 0xFFFFFF;
		}
		return sum;
	}

	static function generate(seed:Int, nfuns:Int) {
		var b = new StringBuf();
		var vars = ["position", "normal", "color", "uv", "time", "scale"];
		var types = ["vec3", "vec3", "vec4", "vec2", "float", "float"];
		for (i in 0...vars.length)
			b.add('var ${vars[i]}_$seed : ${types[i]};\n');
		for (f in 0...nfuns) {
			b.add('function f${f}_$seed(a : float, b : vec3) : vec3 {\n');
			b.add('  var t : float = a * time_$seed + ${f}.5;\n');
			b.add('  var p : vec3 = position_$seed * scale_$seed + b;\n');
			b.add('  if (t > 1.0) { p = p + normal_$seed * t; } else { p = p - normal_$seed; }\n');
			if (f > 0)
				b.add('  p = f${f - 1}_$seed(t, p) + p;\n');
			b.add('  return p * (t + uv_$seed.x);\n}\n');
		}
		return b.toString();
	}

	static function parse(src:String) {
		return new Parser(new Lexer(src).tokens()).parseFile();
	}
}

private enum Token {
	TId(s:String);
	TNum(f:Float);
	TOp(s:String);
	TPOpen;
	TPClose;
	TBrOpen;
	TBrClose;
	TColon;
	TSemicolon;
	TComma;
	TDot;
	TEof;
}

private class Lexer {
	var s:String;
	var pos:Int;

	public function new(s) {
		this.s = s;
		pos = 0;
	}

	public function tokens() {
		var out = [];
		while (true) {
			var t = next();
			out.push(t);
			if (t == TEof)
				break;
		}
		return out;
	}

	function next() {
		while (pos < s.length) {
			var c = s.fastCodeAt(pos);
			if (c == " ".code || c == "\n".code) {
				pos++;
				continue;
			}
			if (isIdent(c)) {
				var start = pos;
				while (pos < s.length && (isIdent(s.fastCodeAt(pos)) || isDigit(s.fastCodeAt(pos))))
					pos++;
				return TId(s.substr(start, pos - start));
			}
			if (isDigit(c)) {
				var start = pos;
				while (pos < s.length && (isDigit(s.fastCodeAt(pos)) || s.fastCodeAt(pos) == ".".code))
					pos++;
				return TNum(Std.parseFloat(s.substr(start, pos - start)));
			}
			pos++;
			return switch (c) {
				case "(".code: TPOpen;
				case ")".code: TPClose;
				case "{".code: TBrOpen;
				case "}".code: TBrClose;
				case ":".code: TColon;
				case ";".code: TSemicolon;
				case ",".code: TComma;
				case ".".code: TDot;
				case _: TOp(String.fromCharCode(c));
			}
		}
		return TEof;
	}

	static inline function isIdent(c:Int) {
		return (c >= "a".code && c <= "z".code) || (c >= "A".code && c <= "Z".code) || c == "_".code;
	}

	static inline function isDigit(c:Int) {
		return c >= "0".code && c <= "9".code;
	}
}

enum Expr {
	EConst(f:Float);
	EIdent(name:String);
	EField(e:Expr, name:String);
	EBinop(op:String, a:Expr, b:Expr);
	ECall(name:String, args:Array<Expr>);
	EVar(name:String, type:String, init:Null<Expr>);
	EAssign(name:String, e:Expr);
	EIf(cond:Expr, a:Array<Expr>, b:Array<Expr>);
	EReturn(e:Expr);
}

typedef FunDecl = {
	name:String,
	args:Array<{name:String, type:String}>,
	ret:String,
	body:Array<Expr>
}

typedef ShaderFile = {
	vars:Array<{name:String, type:String}>,
	funs:Array<FunDecl>
}

private class Parser {
	var tokens:Array<Token>;
	var pos:Int;

	public function new(tokens) {
		this.tokens = tokens;
		pos = 0;
	}

	inline function peek() {
		return tokens[pos];
	}

	function expect(t:Token) {
		var n = tokens[pos++];
		if (!n.equals(t))
			throw 'Expected $t, got $n';
	}

	function ident() {
		return switch (tokens[pos++]) {
			case TId(s): s;
			case t: throw 'Expected identifier, got $t';
		}
	}

	public function parseFile():ShaderFile {
		var file:ShaderFile = {vars: [], funs: []};
		while (true) {
			switch (peek()) {
				case TEof:
					return file;
				case TId("var"):
					pos++;
					var name = ident();
					expect(TColon);
					file.vars.push({name: name, type: ident()});
					expect(TSemicolon);
				case TId("function"):
					pos++;
					file.funs.push(parseFunction());
				case t:
					throw 'Unexpected $t';
			}
		}
	}

	function parseFunction():FunDecl {
		var name = ident();
		expect(TPOpen);
		var args = [];
		while (peek() != TPClose) {
			var n = ident();
			expect(TColon);
			args.push({name: n, type: ident()});
			if (peek() == TComma)
				pos++;
		}
		pos++;
		expect(TColon);
		var ret = ident();
		return {name: name, args: args, ret: ret, body: parseBlock()};
	}

	function parseBlock() {
		expect(TBrOpen);
		var out = [];
		while (peek() != TBrClose)
			out.push(parseStatement());
		pos++;
		return out;
	}

	function parseStatement():Expr {
		return switch (peek()) {
			case TId("var"):
				pos++;
				var name = ident();
				expect(TColon);
				var type = ident();
				var init = null;
				if (peek().equals(TOp("="))) {
					pos++;
					init = parseExpr();
				}
				expect(TSemicolon);
				EVar(name, type, init);
			case TId("if"):
				pos++;
				expect(TPOpen);
				var cond = parseExpr();
				expect(TPClose);
				var a = parseBlock();
				var b = [];
				if (peek().equals(TId("else"))) {
					pos++;
					b = parseBlock();
				}
				EIf(cond, a, b);
			case TId("return"):
				pos++;
				var e = parseExpr();
				expect(TSemicolon);
				EReturn(e);
			case TId(name) if (tokens[pos + 1].equals(TOp("="))):
				pos += 2;
				var e = parseExpr();
				expect(TSemicolon);
				EAssign(name, e);
			case t:
				throw 'Unexpected $t';
		}
	}

	static function priority(op:String) {
		return switch (op) {
			case "*" | "/": 2;
			case "+" | "-": 1;
			case _: 0;
		}
	}

	function parseExpr(minPrio = 0):Expr {
		var e = parseUnary();
		while (true) {
			switch (peek()) {
				case TOp(op) if (op != "=" && priority(op) >= minPrio):
					pos++;
					e = EBinop(op, e, parseExpr(priority(op) + 1));
				case _:
					return e;
			}
		}
	}

	function parseUnary():Expr {
		var e = switch (tokens[pos++]) {
			case TNum(f): EConst(f);
			case TPOpen:
				var e = parseExpr();
				expect(TPClose);
				e;
			case TId(name) if (peek() == TPOpen):
				pos++;
				var args = [];
				while (peek() != TPClose) {
					args.push(parseExpr());
					if (peek() == TComma)
						pos++;
				}
				pos++;
				ECall(name, args);
			case TId(name): EIdent(name);
			case t: throw 'Unexpected $t';
		}
		while (peek() == TDot) {
			pos++;
			e = EField(e, ident());
		}
		return e;
	}
}

enum Type {
	TFloat;
	TBool;
	TVec(n:Int);
}

typedef TExpr = {
	e:TExprDef,
	t:Type
}

enum TExprDef {
	TConst(f:Float);
	TVar(v:TVarInfo);
	TSwiz(e:TExpr, comps:Array<Int>);
	TBinop(op:String, a:TExpr, b:TExpr);
	TCall(f:TFun, args:Array<TExpr>);
	TDecl(v:TVarInfo, init:Null<TExpr>);
	TAssign(v:TVarInfo, e:TExpr);
	TIf(cond:TExpr, a:Array<TExpr>, b:Array<TExpr>);
	TReturn(e:TExpr);
}

class TVarInfo {
	static var uid = 0;

	public final id:Int;
	public final name:String;
	public final type:Type;
	public final global:Bool;

	public function new(name, type, global) {
		id = uid++;
		this.name = name;
		this.type = type;
		this.global = global;
	}
}

class TFun {
	public final name:String;
	public final args:Array<TVarInfo>;
	public final ret:Type;
	public var body:Array<TExpr>;

	public function new(name, args, ret) {
		this.name = name;
		this.args = args;
		this.ret = ret;
		body = [];
	}
}

typedef CheckedShader = {
	vars:Array<TVarInfo>,
	funs:Array<TFun>
}

private class Checker {
	var scopes:Array<Map<String, TVarInfo>>;
	var funs:Map<String, TFun>;
	var current:TFun;

	function new() {
		scopes = [new Map()];
		funs = new Map();
	}

	public static function check(file:ShaderFile):CheckedShader {
		var c = new Checker();
		var vars = [];
		for (v in file.vars) {
			var info = new TVarInfo(v.name, c.parseType(v.type), true);
			c.scopes[0].set(v.name, info);
			vars.push(info);
		}
		var out = [];
		for (f in file.funs) {
			var tf = new TFun(f.name, [for (a in f.args) new TVarInfo(a.name, c.parseType(a.type), false)], c.parseType(f.ret));
			c.funs.set(f.name, tf);
			out.push(tf);
			c.current = tf;
			c.scopes.push([for (a in tf.args) a.name => a]);
			tf.body = c.block(f.body);
			c.scopes.pop();
		}
		return {vars: vars, funs: out};
	}

	public static function hash(s:String) {
		var h = 5381;
		for (i in 0...s.length)
			h = ((h << 5) + h + s.fastCodeAt(i)) & 0xFFFFFF;
		return h;
	}

	function parseType(s:String) {
		return switch (s) {
			case "float": TFloat;
			case "bool": TBool;
			case "vec2": TVec(2);
			case "vec3": TVec(3);
			case "vec4": TVec(4);
			case _: throw 'Unknown type $s';
		}
	}

	function lookup(name:String) {
		var i = scopes.length - 1;
		while (i >= 0) {
			var v = scopes[i].get(name);
			if (v != null)
				return v;
			i--;
		}
		throw 'Unknown identifier $name';
	}

	function block(el:Array<Expr>) {
		scopes.push(new Map());
		var out = [for (e in el) statement(e)];
		scopes.pop();
		return out;
	}

	function statement(e:Expr):TExpr {
		return switch (e) {
			case EVar(name, type, init):
				var v = new TVarInfo(name, parseType(type), false);
				var ti = init == null ? null : unify(expr(init), v.type);
				scopes[scopes.length - 1].set(name, v);
				{e: TDecl(v, ti), t: v.type};
			case EAssign(name, e):
				var v = lookup(name);
				{e: TAssign(v, unify(expr(e), v.type)), t: v.type};
			case EIf(cond, a, b):
				var c = expr(cond);
				if (c.t != TBool)
					throw "Condition must be bool";
				{e: TIf(c, block(a), block(b)), t: TBool};
			case EReturn(e):
				{e: TReturn(unify(expr(e), current.ret)), t: current.ret};
			case _:
				expr(e);
		}
	}

	function unify(e:TExpr, t:Type) {
		if (!e.t.equals(t))
			throw 'Type mismatch: ${e.t} should be $t';
		return e;
	}

	function expr(e:Expr):TExpr {
		return switch (e) {
			case EConst(f):
				{e: TConst(f), t: TFloat};
			case EIdent(name):
				var v = lookup(name);
				{e: TVar(v), t: v.type};
			case EField(e, name):
				var te = expr(e);
				var comps = [
					for (i in 0...name.length)
						switch (name.charAt(i)) {
							case "x" | "r": 0;
							case "y" | "g": 1;
							case "z" | "b": 2;
							case "w" | "a": 3;
							case c: throw 'Invalid swizzle $c';
						}
				];
				{e: TSwiz(te, comps), t: comps.length == 1 ? TFloat : TVec(comps.length)};
			case EBinop(op, a, b):
				var ta = expr(a), tb = expr(b);
				var t = switch [op, ta.t, tb.t] {
					case [">" | "<", TFloat, TFloat]: TBool;
					case [_, TFloat, TFloat]: TFloat;
					case [_, TVec(n), TFloat] | [_, TFloat, TVec(n)]: TVec(n);
					case [_, TVec(n), TVec(m)] if (n == m): TVec(n);
					case _: throw 'Cannot apply $op to ${ta.t} and ${tb.t}';
				}
				{e: TBinop(op, ta, tb), t: t};
			case ECall(name, args):
				var f = funs.get(name);
				if (f == null)
					throw 'Unknown function $name';
				var targs = [for (i in 0...args.length) unify(expr(args[i]), f.args[i].type)];
				{e: TCall(f, targs), t: f.ret};
			case _:
				throw 'Unexpected $e';
		}
	}
}

private class Linker {
	public static function link(shaders:Array<CheckedShader>) {
		var used = new Map<Int, TVarInfo>();
		var names = new Map<String, Int>();
		var renamed = new Map<Int, String>();
		function name(v:TVarInfo) {
			var n = renamed.get(v.id);
			if (n == null) {
				var count = names.get(v.name);
				names.set(v.name, count == null ? 1 : count + 1);
				n = count == null ? v.name : v.name + count;
				renamed.set(v.id, n);
			}
			return n;
		}
		function typeName(t:Type) {
			return switch (t) {
				case TFloat: "float";
				case TBool: "bool";
				case TVec(n): "vec" + n;
			}
		}
		var b = new StringBuf();
		function print(e:TExpr) {
			switch (e.e) {
				case TConst(f):
					b.add(f);
				case TVar(v):
					if (v.global)
						used.set(v.id, v);
					b.add(name(v));
				case TSwiz(e, comps):
					print(e);
					b.add(".");
					for (c in comps)
						b.add("xyzw".charAt(c));
				case TBinop(op, a, b2):
					b.add("(");
					print(a);
					b.add(op);
					print(b2);
					b.add(")");
				case TCall(f, args):
					b.add(f.name);
					b.add("(");
					for (i in 0...args.length) {
						if (i > 0)
							b.add(",");
						print(args[i]);
					}
					b.add(")");
				case TDecl(v, init):
					b.add(typeName(v.type) + " " + name(v));
					if (init != null) {
						b.add("=");
						print(init);
					}
					b.add(";");
				case TAssign(v, e):
					b.add(name(v) + "=");
					print(e);
					b.add(";");
				case TIf(cond, a, b2):
					b.add("if(");
					print(cond);
					b.add("){");
					for (e in a)
						print(e);
					b.add("}else{");
					for (e in b2)
						print(e);
					b.add("}");
				case TReturn(e):
					b.add("return ");
					print(e);
					b.add(";");
			}
		}
		for (s in shaders)
			for (f in s.funs) {
				b.add(typeName(f.ret) + " " + f.name + "(");
				b.add([for (a in f.args) typeName(a.type) + " " + name(a)].join(","));
				b.add("){");
				for (e in f.body)
					print(e);
				b.add("}\n");
			}
		var header = new StringBuf();
		var ids = [for (id in used.keys()) id];
		ids.sort((a, b) -> a - b);
		for (id in ids) {
			var v = used.get(id);
			header.add("uniform " + typeName(v.type) + " " + name(v) + ";\n");
		}
		return header.toString() + b.toString();
	}
}
