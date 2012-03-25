/*
 * Copyright (c) 2005-2010, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
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
	var packages : Hash<Bool>;
	var forbidden : Hash<Bool>;

	public function new(api) {
		this.api = api;
		buf = new StringBuf();
		inits = new List();
		statics = new List();
		packages = new Hash();
		forbidden = new Hash();
		for( x in ["prototype", "__proto__", "constructor"] )
			forbidden.set(x, true);
		api.setTypeAccessor(getType);
	}

	function getType( t : Type ) {
		return switch(t) {
			case TInst(c, _): getPath(c.get());
			case TEnum(e, _): getPath(e.get());
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

	@:macro static function fprint( e : Expr ) {
		var pos = haxe.macro.Context.currentPos();
		var ret = haxe.macro.Format.format(e);
		return { expr : ECall({ expr : EConst(CIdent("print")), pos : pos },[ret]), pos : pos };
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
				fprint("if(typeof $x=='undefined') $x = {}");
			else {
				var p = prev + field(x);
				fprint("if(!$p) $p = {}");
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
		fprint("$p.prototype$field = ");
		if( f.expr == null )
			print("null");
		else {
			genExpr(f.expr);
		}
		newline();
	}

	function genStaticField( c : ClassType, p : String, f : ClassField ) {
		checkFieldName(c, f);
		var field = field(f.name);
		if( f.expr == null ) {
			fprint("$p$field = null");
			newline();
		} else switch( f.kind ) {
		case FMethod(_):
			fprint("$p$field = ");
			genExpr(f.expr);
			newline();
		default:
			statics.add( { c : c, f : f } );
		}
	}

	function genClass( c : ClassType ) {
		genPackage(c.pack);
		api.setCurrentClass(c);
		var p = getPath(c);
		fprint("$p = ");
		if( c.constructor != null )
			genExpr(c.constructor.get().expr);
		else
			print("function() { }");
		newline();
		var name = p.split(".").map(api.quoteString).join(",");
		fprint("$p.__name__ = [$name]");
		newline();
		if( c.superClass != null ) {
			var psup = getPath(c.superClass.t.get());
			fprint("$p.__super__ = $psup");
			newline();
			fprint("for(var k in $psup.prototype ) $p.prototype[k] = $psup.prototype[k]");
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
		fprint("$p.prototype.__class__ = $p");
		newline();
		if( c.interfaces.length > 0 ) {
			var me = this;
			var inter = c.interfaces.map(function(i) return me.getPath(i.t.get())).join(",");
			fprint("$p.__interfaces__ = [$inter]");
			newline();
		}
	}

	function genEnum( e : EnumType ) {
		genPackage(e.pack);
		var p = getPath(e);
		var names = p.split(".").map(api.quoteString).join(",");
		var constructs = e.names.map(api.quoteString).join(",");
		fprint("$p = { __ename__ : [$names], __constructs__ : [$constructs] }");
		newline();
		for( c in e.constructs.keys() ) {
			var c = e.constructs.get(c);
			var f = field(c.name);
			fprint("$p$f = ");
			switch( c.type ) {
			case TFun(args, _):
				var sargs = args.map(function(a) return a.name).join(",");
				fprint('function($sargs) { var $$x = ["${c.name}",${c.index},$sargs]; $$x.__enum__ = $p; $$x.toString = $$estr; return $$x; }');
			default:
				print("[" + api.quoteString(c.name) + "," + c.index + "]");
				newline();
				fprint("$p$f.toString = $$estr");
				newline();
				fprint("$p$f.__enum__ = $$p");
			}
			newline();
		}
		var meta = api.buildMetaData(e);
		if( meta != null ) {
			fprint("$p.__meta__ = ");
			genExpr(meta);
			newline();
		}
	}


	function genStaticValue( c : ClassType, cf : ClassField ) {
		var p = getPath(c);
		var f = field(cf.name);
		fprint("$p$f = ");
		genExpr(cf.expr);
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
		for( t in api.types )
			genType(t);
		print("$_ = {}");
		newline();
		print("js.Boot.__res = {}");
		newline();
		print("js.Boot.__init()");
		newline();
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
		var file = neko.io.File.write(api.outputFile, true);
		file.writeString(buf.toString());
		file.close();
	}

	#if macro
	public static function use() {
		Compiler.setCustomJSGenerator(function(api) new ExampleJSGenerator(api).generate());
	}
	#end

}
