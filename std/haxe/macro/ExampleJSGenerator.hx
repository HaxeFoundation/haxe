/*
 * Copyright (C)2005-2017 Haxe Foundation
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
import haxe.macro.Type;
import haxe.macro.Expr;
using Lambda;

class ExampleJSGenerator {

	var api : JSGenApi;
	var buf : StringBuf;
	var inits : List<TypedExpr>;
	var statics : List<{ c : ClassType, f : ClassField }>;
	var packages : haxe.ds.StringMap<Bool>;
	var forbidden : haxe.ds.StringMap<Bool>;

	public function new(api) {
		this.api = api;
		buf = new StringBuf();
		inits = new List();
		statics = new List();
		packages = new haxe.ds.StringMap();
		forbidden = new haxe.ds.StringMap();
		for( x in ["prototype", "__proto__", "constructor"] )
			forbidden.set(x, true);
		api.setTypeAccessor(getType);
	}

	function getType( t : Type ) {
		return switch(t) {
			case TInst(c, _): getPath(c.get());
			case TEnum(e, _): getPath(e.get());
			case TAbstract(a, _): getPath(a.get());
			default: throw "assert";
		};
	}

	inline function print(str) {
		buf.add(str);
	}

	inline function newline() {
		buf.add(";\n");
	}

	inline function genExpr(e) {
		print(api.generateValue(e));
	}

	function field(p) {
		return api.isKeyword(p) ? '["' + p + '"]' : "." + p;
	}

	function genPackage( p : Array<String> ) {
		var full = null;
		for( x in p ) {
			var prev = full;
			if( full == null ) full = x else full += "." + x;
			if( packages.exists(full) )
				continue;
			packages.set(full, true);
			if( prev == null )
				print('if(typeof $x==\'undefined\') $x = {}');
			else {
				var p = prev + field(x);
				print('if(!$p) $p = {}');
			}
			newline();
		}
	}

	function getPath( t : BaseType ) {
		return (t.pack.length == 0) ? t.name : t.pack.join(".") + "." + t.name;
	}

	function checkFieldName( c : ClassType, f : ClassField ) {
		if( forbidden.exists(f.name) )
			Context.error("The field " + f.name + " is not allowed in JS", c.pos);
	}

	function genClassField( c : ClassType, p : String, f : ClassField ) {
		checkFieldName(c, f);
		var field = field(f.name);
		print('$p.prototype$field = ');
		var e = f.expr();
		if( e == null )
			print("null");
		else {
			genExpr(e);
		}
		newline();
	}

	function genStaticField( c : ClassType, p : String, f : ClassField ) {
		checkFieldName(c, f);
		var field = field(f.name);
		var e = f.expr();
		if( e == null ) {
			print('$p$field = null');
			newline();
		} else switch( f.kind ) {
		case FMethod(_):
			print('$p$field = ');
			genExpr(e);
			newline();
		default:
			statics.add( { c : c, f : f } );
		}
	}

	function genClass( c : ClassType ) {
		genPackage(c.pack);
		api.setCurrentClass(c);
		var p = getPath(c);
		print('$p = $$hxClasses[\'$p\'] = ');
		if( c.constructor != null )
			genExpr(c.constructor.get().expr());
		else
			print("function() { }");
		newline();
		var name = p.split(".").map(api.quoteString).join(",");
		print('$p.__name__ = [$name]');
		newline();
		if( c.superClass != null ) {
			var psup = getPath(c.superClass.t.get());
			print('$p.__super__ = $psup');
			newline();
			print('for(var k in $psup.prototype ) $p.prototype[k] = $psup.prototype[k]');
			newline();
		}
		for( f in c.statics.get() )
			genStaticField(c, p, f);
		for( f in c.fields.get() ) {
			switch( f.kind ) {
			case FVar(r, _):
				if( r == AccResolve ) continue;
			default:
			}
			genClassField(c, p, f);
		}
		print('$p.prototype.__class__ = $p');
		newline();
		if( c.interfaces.length > 0 ) {
			var me = this;
			var inter = c.interfaces.map(function(i) return me.getPath(i.t.get())).join(",");
			print('$p.__interfaces__ = [$inter]');
			newline();
		}
	}

	function genEnum( e : EnumType ) {
		genPackage(e.pack);
		var p = getPath(e);
		var names = p.split(".").map(api.quoteString).join(",");
		var constructs = e.names.map(api.quoteString).join(",");
		print('$p = $$hxClasses[\'$p\'] = { __ename__ : [$names], __constructs__ : [$constructs] }');
		newline();
		for( c in e.constructs.keys() ) {
			var c = e.constructs.get(c);
			var f = field(c.name);
			print('$p$f = ');
			switch( c.type ) {
			case TFun(args, _):
				var sargs = args.map(function(a) return a.name).join(",");
				print('function($sargs) { var $$x = ["${c.name}",${c.index},$sargs]; $$x.__enum__ = $p; $$x.toString = $$estr; return $$x; }');
			default:
				print("[" + api.quoteString(c.name) + "," + c.index + "]");
				newline();
				print('$p$f.toString = $$estr');
				newline();
				print('$p$f.__enum__ = $p');
			}
			newline();
		}
		var meta = api.buildMetaData(e);
		if( meta != null ) {
			print('$p.__meta__ = ');
			genExpr(meta);
			newline();
		}
	}


	function genStaticValue( c : ClassType, cf : ClassField ) {
		var p = getPath(c);
		var f = field(cf.name);
		print('$p$f = ');
		genExpr(cf.expr());
		newline();
	}

	function genType( t : Type ) {
		switch( t ) {
		case TInst(c, _):
			var c = c.get();
			if( c.init != null )
				inits.add(c.init);
			if( !c.isExtern ) genClass(c);
		case TEnum(r, _):
			var e = r.get();
			if( !e.isExtern ) genEnum(e);
		default:
		}
	}

	public function generate() {
		print("var $_, $hxClasses = $hxClasses || {}, $estr = function() { return js.Boot.__string_rec(this,''); }");
		newline();
		print("function $bind(o,m) { var f = function(){ return f.method.apply(f.scope, arguments); }; f.scope = o; f.method = m; return f; };");
		newline();
		for( t in api.types )
			genType(t);
		for( e in inits ) {
			print(api.generateStatement(e));
			newline();
		}
		for( s in statics ) {
			genStaticValue(s.c,s.f);
			newline();
		}
		if( api.main != null ) {
			genExpr(api.main);
			newline();
		}
		sys.io.File.saveContent(api.outputFile, buf.toString());
	}

	#if macro
	public static function use() {
		Compiler.setCustomJSGenerator(function(api) new ExampleJSGenerator(api).generate());
	}
	#end

}
