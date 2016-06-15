/*
 * Copyright (C)2005-2016 Haxe Foundation
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
package haxe;

private enum TemplateExpr {
	OpVar( v : String );
	OpExpr( expr :  Void -> Dynamic );
	OpIf( expr : Void -> Dynamic,  eif : TemplateExpr, eelse : TemplateExpr );
	OpStr( str : String );
	OpBlock( l : List<TemplateExpr> );
	OpForeach( expr : Void -> Dynamic, loop : TemplateExpr );
	OpMacro( name : String, params : List<TemplateExpr> );
}

private typedef Token = {
	var s : Bool;
	var p : String;
	var l : Array<String>;
}

private typedef ExprToken = {
	var s : Bool;
	var p : String;
}

/**
	Template provides a basic templating mechanism to replace values in a source
	String, and to have some basic logic.

	A complete documentation of the supported syntax is available at:
	<http://haxe.org/manual/std-template.html>
**/
class Template {

	static var splitter = ~/(::[A-Za-z0-9_ ()&|!+=\/><*."-]+::|\$\$([A-Za-z0-9_-]+)\()/;
	static var expr_splitter = ~/(\(|\)|[ \r\n\t]*"[^"]*"[ \r\n\t]*|[!+=\/><*.&|-]+)/;
	static var expr_trim = ~/^[ ]*([^ ]+)[ ]*$/;
	static var expr_int = ~/^[0-9]+$/;
	static var expr_float = ~/^([+-]?)(?=\d|,\d)\d*(,\d*)?([Ee]([+-]?\d+))?$/;

	/**
		Global replacements which are used across all Template instances. This
		has lower priority than the context argument of execute().
	**/
	public static var globals : Dynamic = {};

	var expr : TemplateExpr;
	var context : Dynamic;
	var macros : Dynamic;
	var stack : List<Dynamic>;
	var buf : StringBuf;

	/**
		Creates a new Template instance from `str`.

		`str` is parsed into tokens, which are stored for internal use. This
		means that multiple execute() operations on a single Template instance
		are more efficient than one execute() operations on multiple Template
		instances.

		If `str` is null, the result is unspecified.
	**/
	public function new( str : String ) {
		var tokens = parseTokens(str);
		expr = parseBlock(tokens);
		if( !tokens.isEmpty() )
			throw "Unexpected '"+tokens.first().s+"'";
	}

	/**
		Executes `this` Template, taking into account `context` for
		replacements and `macros` for callback functions.

		If `context` has a field 'name', its value replaces all occurrences of
		::name:: in the Template. Otherwise Template.globals is checked instead,
		If 'name' is not a field of that either, ::name:: is replaced with null.

		If `macros` has a field 'name', all occurrences of $$name(args) are
		replaced with the result of calling that field. The first argument is
		always the resolve() method, followed by the given arguments.
		If `macros` has no such field, the result is unspecified.

		If `context` is null, the result is unspecified. If `macros` is null,
		no macros are used.
	**/
	public function execute( context : Dynamic, ?macros : Dynamic ):String {
		this.macros = if( macros == null ) {} else macros;
		this.context = context;
		stack = new List();
		buf = new StringBuf();
		run(expr);
		return buf.toString();
	}

	function resolve( v : String ) : Dynamic {
		if( v == "__current__" )
			return context;
		var value = Reflect.getProperty(context, v);
		if( value != null || Reflect.hasField(context,v) )
			return value;
		for( ctx in stack ) {
			value = Reflect.getProperty(ctx,v);
			if( value != null || Reflect.hasField(ctx,v) )
				return value;
		}
		return Reflect.field(globals,v);
	}

	function parseTokens( data : String ) {
		var tokens = new List<Token>();
		while( splitter.match(data) ) {
			var p = splitter.matchedPos();
			if( p.pos > 0 )
				tokens.add({ p : data.substr(0,p.pos), s : true, l : null });

			// : ?
			if( data.charCodeAt(p.pos) == 58 ) {
				tokens.add({ p : data.substr(p.pos + 2,p.len - 4), s : false, l : null });
				data = splitter.matchedRight();
				continue;
			}

			// macro parse
			var parp = p.pos + p.len;
			var npar = 1;
			var params = [];
			var part = "";
			while( true ) {
				var c = data.charCodeAt(parp);
				parp++;
				if( c == 40 ) {
					npar++;
				} else if( c == 41 ) {
					npar--;
					if (npar <= 0) break;
				} else if( c == null ){
					throw "Unclosed macro parenthesis";
				}
				if ( c == 44 && npar == 1) {
					params.push(part);
					part = "";
				} else {
					part += String.fromCharCode(c);
				}
			}
			params.push(part);
			tokens.add({ p : splitter.matched(2), s : false, l : params });
			data = data.substr(parp,data.length - parp);
		}
		if( data.length > 0 )
			tokens.add({ p : data, s : true, l : null });
		return tokens;
	}

	function parseBlock( tokens : List<Token> ) {
		var l = new List();
		while( true ) {
			var t = tokens.first();
			if( t == null )
				break;
			if( !t.s && (t.p == "end" || t.p == "else" || t.p.substr(0,7) == "elseif ") )
				break;
			l.add(parse(tokens));
		}
		if( l.length == 1 )
			return l.first();
		return OpBlock(l);
	}

	function parse( tokens : List<Token> ) {
		var t = tokens.pop();
		var p = t.p;
		if( t.s )
			return OpStr(p);
		// macro
		if( t.l != null ) {
			var pe = new List();
			for( p in t.l )
				pe.add(parseBlock(parseTokens(p)));
			return OpMacro(p,pe);
		}
		// 'end' , 'else', 'elseif' can't be found here
		if( p.substr(0,3) == "if " ) {
			p = p.substr(3,p.length - 3);
			var e = parseExpr(p);
			var eif = parseBlock(tokens);
			var t = tokens.first();
			var eelse;
			if( t == null )
				throw "Unclosed 'if'";
			if( t.p == "end" ) {
				tokens.pop();
				eelse = null;
			} else if( t.p == "else" ) {
				tokens.pop();
				eelse = parseBlock(tokens);
				t = tokens.pop();
				if( t == null || t.p != "end" )
					throw "Unclosed 'else'";
			} else { // elseif
				t.p = t.p.substr(4,t.p.length - 4);
				eelse = parse(tokens);
			}
			return OpIf(e,eif,eelse);
		}
		if( p.substr(0,8) == "foreach " ) {
			p = p.substr(8,p.length - 8);
			var e = parseExpr(p);
			var efor = parseBlock(tokens);
			var t = tokens.pop();
			if( t == null || t.p != "end" )
				throw "Unclosed 'foreach'";
			return OpForeach(e,efor);
		}
		if( expr_splitter.match(p) )
			return OpExpr(parseExpr(p));
		return OpVar(p);
	}

	function parseExpr( data : String ) {
		var l = new List<ExprToken>();
		var expr = data;
		while( expr_splitter.match(data) ) {
			var p = expr_splitter.matchedPos();
			var k = p.pos + p.len;
			if( p.pos != 0 )
				l.add({ p : data.substr(0,p.pos), s : true });
			var p = expr_splitter.matched(0);
			l.add({ p : p, s : p.indexOf('"') >= 0 });
			data = expr_splitter.matchedRight();
		}
		if( data.length != 0 )
			l.add({ p : data, s : true });
		var e:Void->Dynamic;
		try {
			e = makeExpr(l);
			if( !l.isEmpty() )
				throw l.first().p;
		} catch( s : String ) {
			throw "Unexpected '"+s+"' in "+expr;
		}
		return function() {
			try {
				return e();
			} catch( exc : Dynamic ) {
				throw "Error : "+Std.string(exc)+" in "+expr;
			}
		}
	}

	function makeConst( v : String ) : Void -> Dynamic {
		expr_trim.match(v);
		v = expr_trim.matched(1);
		if( v.charCodeAt(0) == 34 ) {
			var str = v.substr(1,v.length-2);
			return function() return str;
		}
		if( expr_int.match(v) ) {
			var i = Std.parseInt(v);
			return function() { return i; };
		}
		if( expr_float.match(v) ) {
			var f = Std.parseFloat(v);
			return function() { return f; };
		}
		var me = this;
		return function() { return me.resolve(v); };
	}

	function makePath( e : Void -> Dynamic, l : List<ExprToken> ) {
		var p = l.first();
		if( p == null || p.p != "." )
			return e;
		l.pop();
		var field = l.pop();
		if( field == null || !field.s )
			throw field.p;
		var f = field.p;
		expr_trim.match(f);
		f = expr_trim.matched(1);
		return makePath(function() { return Reflect.field(e(),f); },l);
	}

	function makeExpr( l ) {
		return makePath(makeExpr2(l),l);
	}

	function makeExpr2( l : List<ExprToken> ) : Void -> Dynamic {
		var p = l.pop();
		if( p == null )
			throw "<eof>";
		if( p.s )
			return makeConst(p.p);
		switch( p.p ) {
		case "(":
			var e1:Dynamic = makeExpr(l);
			var p = l.pop();
			if( p == null || p.s )
				throw p;
			if( p.p == ")" )
				return e1;
			var e2:Dynamic = makeExpr(l);
			var p2 = l.pop();
			if( p2 == null || p2.p != ")" )
				throw p2;
			return switch( p.p ) {
			case "+": function() { return cast e1() + e2(); };
			case "-": function() { return cast e1() - e2(); };
			case "*": function() { return cast e1() * e2(); };
			case "/": function() { return cast e1() / e2(); };
			case ">": function() { return cast e1() > e2(); };
			case "<": function() { return cast e1() < e2(); };
			case ">=": function() { return cast e1() >= e2(); };
			case "<=": function() { return cast e1() <= e2(); };
			case "==": function() { return cast e1() == e2(); };
			case "!=": function() { return cast e1() != e2(); };
			case "&&": function() { return cast e1() && e2(); };
			case "||": function() { return cast e1() || e2(); };
			default: throw "Unknown operation "+p.p;
			}
		case "!":
			var e : Void->Dynamic = makeExpr(l);
			return function() {
				var v : Dynamic = e();
				return (v == null || v == false);
			};
		case "-":
			var e = makeExpr(l);
			return function() { return -e(); };
		}
		throw p.p;
	}

	function run( e : TemplateExpr ) {
		switch( e ) {
		case OpVar(v):
			buf.add(Std.string(resolve(v)));
		case OpExpr(e):
			buf.add(Std.string(e()));
		case OpIf(e,eif,eelse):
			var v : Dynamic = e();
			if( v == null || v == false ) {
				if( eelse != null ) run(eelse);
			} else
				run(eif);
		case OpStr(str):
			buf.add(str);
		case OpBlock(l):
			for( e in l )
				run(e);
		case OpForeach(e,loop):
			var v : Dynamic = e();
			try {
				var x : Dynamic = v.iterator();
				if( x.hasNext == null ) throw null;
				v = x;
			} catch( e : Dynamic ) try {
				if( v.hasNext == null ) throw null;
			} catch( e : Dynamic ) {
				throw "Cannot iter on " + v;
			}
			stack.push(context);
			var v : Iterator<Dynamic> = v;
			for( ctx in v ) {
				context = ctx;
				run(loop);
			}
			context = stack.pop();
		case OpMacro(m,params):
			var v : Dynamic = Reflect.field(macros,m);
			var pl = new Array<Dynamic>();
			var old = buf;
			pl.push(resolve);
			for( p in params ) {
				switch( p ) {
				case OpVar(v): pl.push(resolve(v));
				default:
					buf = new StringBuf();
					run(p);
					pl.push(buf.toString());
				}
			}
			buf = old;
			try {
				buf.add(Std.string(Reflect.callMethod(macros,v,pl)));
			} catch( e : Dynamic ) {
				var plstr = try pl.join(",") catch( e : Dynamic ) "???";
				var msg = "Macro call "+m+"("+plstr+") failed ("+Std.string(e)+")";
				#if neko
				neko.Lib.rethrow(msg);
				#else
				throw msg;
				#end
			}
		}
	}

}
