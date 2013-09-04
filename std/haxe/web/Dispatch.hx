/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package haxe.web;

#if macro
import haxe.macro.Expr;
import haxe.macro.Type.ClassField;
import haxe.macro.Context;
#end

typedef DispatchConfig = {
	var obj : Dynamic;
	var rules : Dynamic;
}

typedef Lock<T> = T;

enum MatchRule {
	MRInt;
	MRBool;
	MRFloat;
	MRString;
	MRDispatch;
	MRSpod( c : String, lock : Bool );
	MROpt( r : MatchRule );
}

enum DispatchRule {
	DRMatch( r : MatchRule );
	DRMult( r : Array<MatchRule> );
	DRArgs( r : DispatchRule, args : Array<{ name : String, rule : MatchRule, opt : Bool }>, opt : Bool );
	DRMeta( r : DispatchRule );
}

enum DispatchError {
	DENotFound( part : String );
	DEInvalidValue;
	DEMissing;
	DEMissingParam( p : String );
	DETooManyValues;
}

class Redirect {
	public function new() {
	}
}

class Dispatch {

	public var parts : Array<String>;
	public var params : haxe.ds.StringMap<String>;
	public var name : String;
	public var cfg : DispatchConfig;
	var subDispatch : Bool;

	public function new(url:String, params) {
		parts = url.split("/");
		if( parts[0] == "" ) parts.shift();
		this.params = params;
	}

	public macro function dispatch( ethis : Expr, obj : ExprOf<{}> ) : ExprOf<Void> {
		var p = Context.currentPos();
		var cfg = makeConfig(obj);
		return { expr : ECall({ expr : EField(ethis, "runtimeDispatch"), pos : p }, [cfg]), pos : p };
	}

	public macro function getParams( ethis : Expr ) : Expr {
		var p = Context.currentPos();
		if( PARAMS == null ) {
			PARAMS = new Array();
			Context.onGenerate(buildParams);
		}
		var index = PARAMS.length;
		var t = Context.typeof({ expr : EConst(CIdent("null")), pos : p });
		PARAMS.push( { p : p, t : t } );
		var call = { expr : ECall( { expr : EField(ethis, "runtimeGetParams"), pos : p }, [ { expr : EConst(CInt(Std.string(index))), pos : p } ]), pos : p };
		var rt = TPath( { pack : ["haxe", "macro"], name : "MacroType", params : [TPExpr(Context.parse("haxe.web.Dispatch.getRunParam("+index+")",p))], sub : null } );
		return { expr : EBlock([ { expr : EVars([ { name : "tmp", type : rt, expr : call } ]), pos : p }, { expr : EConst(CIdent("tmp")), pos : p } ]), pos : p };
	}

	public dynamic function onMeta( v : String, args : Null<Array<Dynamic>> ) {
	}

	function resolveName( name : String ) {
		return name;
	}

	public function runtimeDispatch( cfg : DispatchConfig ) {
		name = parts.shift();
		if( name == null )
			name = "default";
		name = resolveName(name);
		this.cfg = cfg;
		var r : DispatchRule = Reflect.field(cfg.rules, name);
		if( r == null ) {
			r = Reflect.field(cfg.rules, "default");
			if( r == null )
				throw DENotFound(name);
			parts.unshift(name);
			name = "default";
		}
		name = "do" + name.charAt(0).toUpperCase() + name.substr(1);
		var args = [];
		subDispatch = false;
		loop(args, r);
		if( parts.length > 0 && !subDispatch ) {
			if( parts.length == 1 && parts[parts.length - 1] == "" ) parts.pop() else throw DETooManyValues;
		}
		try {
			Reflect.callMethod(cfg.obj, Reflect.field(cfg.obj, name), args);
		} catch( e : Redirect ) {
			runtimeDispatch(cfg);
		}
	}

	public function redirect( url : String, ?params : haxe.ds.StringMap<String> ) {
		parts = url.split("/");
		if( parts[0] == "" ) parts.shift();
		if( params != null ) this.params = params;
		throw new Redirect();
	}

	static var GET_RULES;
	public function runtimeGetParams( cfgIndex : Int ) : Dynamic {
		if( GET_RULES == null )
			GET_RULES = haxe.Unserializer.run(haxe.rtti.Meta.getType(Dispatch).getParams[0]);
		return checkParams(GET_RULES[cfgIndex], true);
	}

	function match( v : String, r : MatchRule, opt : Bool ) : Dynamic {
		switch( r ) {
		case MRInt:
			if( v == null ) throw DEMissing;
			if( opt && v == "" ) return null;
			var v = Std.parseInt(v);
			if( v == null ) throw DEInvalidValue;
			return v;
		case MRFloat:
			if( v == null ) throw DEMissing;
			if( opt && v == "" ) return null;
			var v = Std.parseFloat(v);
			if( Math.isNaN(v) ) throw DEInvalidValue;
			return v;
		case MRString:
			if( v == null ) throw DEMissing;
			return v;
		case MRBool:
			return v != null && v != "0" && v != "false" && v != "null";
		case MRDispatch:
			if( v != null )
				parts.unshift(v);
			subDispatch = true;
			return this;
		case MRSpod(c, lock):
			if( v == null ) throw DEMissing;
			var v = Std.parseInt(v);
			if( v == null ) throw DEInvalidValue;
			var cl : Dynamic = Type.resolveClass(c);
			if( cl == null ) throw "assert";
			var o : Dynamic;
			#if !old_spod
			o = cl.manager.unsafeGet(v, lock);
			#else
			o = cl.manager.get(v, lock);
			#end
			if( o == null ) throw DEInvalidValue;
			return o;
		case MROpt(r) :
			if( v == null )
				return null;
			return match(v, r, true);
		}
	}

	function checkParams( params : Array<{ name : String, opt : Bool, rule : MatchRule }>, opt ) {
		var po = { };
		for( p in params ) {
			var v = this.params.get(p.name);
			if( v == null ) {
				if( p.opt ) continue;
				if( opt ) return null;
				throw DEMissingParam(p.name);
			}
			Reflect.setField(po, p.name, match(v, p.rule,p.opt));
		}
		return po;
	}

	function loop( args : Array<Dynamic>, r ) {
		switch( r ) {
		case DRArgs(r, params, opt):
			loop(args, r);
			args.push( checkParams(params, opt) );
		case DRMatch(r):
			args.push(match(parts.shift(), r, false));
		case DRMult(rl):
			for( r in rl )
				args.push(match(parts.shift(), r, false));
		case DRMeta(r):
			loop(args, r);
			var c = Type.getClass(cfg.obj);
			var m;
			do {
				if( c == null ) throw "assert";
				m = Reflect.field(haxe.rtti.Meta.getFields(c), name);
				c = Type.getSuperClass(c);
			} while( m == null );
			for( mv in Reflect.fields(m) )
				onMeta(mv, Reflect.field(m, mv));
		}
	}

	#if macro

	static inline var PREFIX = "haxe.web.Dispatch.DispatchRule.";

	static function getType(t,p) {
		switch( Context.follow(t) ) {
		case TInst(i,_):
			switch( i.toString() ) {
			case "Int":
				return MRInt;
			case "Float":
				return MRFloat;
			case "String":
				return MRString;
			case "haxe.web.Dispatch":
				return MRDispatch;
			default:
				var c = i.get();
				var csup = c.superClass;
				while( csup != null ) {
					var name = csup.t.toString();
					if( name == "neko.db.Object" || name == "sys.db.Object" ) {
						var lock = switch( t ) {
						case TType(t, _): t.get().name == "Lock";
						default: false;
						}
						return MRSpod(i.toString(), lock);
					}
					else if ( name == "haxe.web.Dispatch" ) {
						return MRDispatch;
					}
					csup = csup.t.get().superClass;
				}
				Context.error("Unsupported dispatch type '"+i.toString()+"'",p);
			}
		case TEnum(e, _):
			switch( e.toString() ) {
			case "Bool":
				return MRBool;
			default:
				Context.error("Unsupported dispatch type "+e.toString(),p);
			}
		case TAbstract(a,_):
			switch( a.toString() ) {
			case "Int":
				return MRInt;
			case "Float":
				return MRFloat;
			case "Bool":
				return MRBool;
			default:
				Context.error("Unsupported dispatch type "+a.toString(),p);
			}
		default:
			Context.error("Unsupported dispatch type "+Std.string(t),p);
		}
		return null;
	}

	static function makeArgs( t : haxe.macro.Type, p ) {
		var args = [];
		switch( Context.follow(t) ) {
		case TAnonymous(a):
			for( f in a.get().fields ) {
				var r = getType(f.type, f.pos);
				var opt = false;
				switch( f.type ) {
				case TType(t, _):
					if( t.get().name == "Null" ) opt = true;
				default:
				}
				if( r == MRBool ) opt = true;
				args.push( { name : f.name, rule : r, opt : opt } );
			}
		default:
			Context.error("Arguments should be an anonymous object", p);
		}
		return args;
	}

	static function makeRule( f : ClassField ) : DispatchRule {
		switch( Context.follow(f.type) ) {
		case TFun(pl, _):
			var params = [];
			var rules = [];
			var args = null, argsOpt = false;
			for( p in pl ) {
				if( p.name == "args" ) {
					if( args != null )
						Context.error("Duplicate arguments", f.pos);
					argsOpt = p.opt;
					args = makeArgs(p.t,f.pos);
					continue;
				}
				if( args != null ) Context.error("Arguments should be last parameter", f.pos);
				var r = getType(p.t, f.pos);
				if( p.opt ) r = MROpt(r);
				rules.push(r);
			}
			var rule = if( rules.length == 1 )
				DRMatch(rules[0]);
			else
				DRMult(rules);
			if( args != null )
				rule = DRArgs(rule, args, argsOpt);
			return rule;
		default:
			Context.error("Configuration entry should be a function", f.pos);
		}
		return null;
	}

	static function makeConfig( obj : Expr ) {
		var p = obj.pos;
		if( Context.defined("display") )
			return { expr :  EObjectDecl([ { field : "obj", expr : obj }, { field : "rules", expr : { expr : EObjectDecl([]), pos : p } } ]), pos : p };
		var t = Context.typeof(obj);
		switch( Context.follow(t) ) {
		case TAnonymous(fl):
			var fields = [];
			for( f in fl.get().fields ) {
				if( f.name.substr(0, 2) != "do" )
					continue;
				if (!f.meta.has(':keep'))
					f.meta.add(':keep', [], f.pos);
				var r = makeRule(f);
				fields.push( { field : f.name.charAt(2).toLowerCase() + f.name.substr(3), expr : Context.makeExpr(r,p) } );
			}
			if( fields.length == 0 )
				Context.error("No dispatch method found", p);
			var rules = { expr : EObjectDecl(fields), pos : p };
			return { expr : EObjectDecl([ { field : "obj", expr : obj }, { field : "rules", expr : rules } ]), pos : p };
		case TInst(i, _):
			var i = i.get();
			// store the config inside the class metadata (only once)
			if( !i.meta.has("dispatchConfig") ) {
				var fields = {};
				var tmp = i;
				while( true ) {
					for( f in tmp.fields.get() ) {
						if( f.name.substr(0, 2) != "do" )
							continue;
						if (!f.meta.has(':keep'))
							f.meta.add(':keep', [], f.pos);
						var r = makeRule(f);
						for( m in f.meta.get() )
							if( m.name.charAt(0) != ":" ) {
								checkMeta(f);
								r = DRMeta(r);
								break;
							}
						Reflect.setField(fields, f.name.charAt(2).toLowerCase() + f.name.substr(3), r);
					}
					if( tmp.superClass == null )
						break;
					tmp = tmp.superClass.t.get();
				}
				if( Reflect.fields(fields).length == 0 )
					Context.error("No dispatch method found", p);
				var str = serialize(fields);
				i.meta.add("dispatchConfig", [ { expr : EConst(CString(str)), pos : p } ], p);
			}
			return { expr : EUntyped ({ expr : ECall({ expr : EField(Context.makeExpr(Dispatch,p),"extractConfig"), pos : p },[obj]), pos : p }), pos : p };
		default:
			Context.error("Configuration should be an anonymous object",p);
		}
		return null;
	}

	static var PARAMS:Array<Dynamic> = null;

	static function buildParams(_) {
		var rules = [];
		for( p in PARAMS )
			rules.push(makeArgs(p.t, p.p));
		var str = serialize(rules);
		switch( Context.getType("haxe.web.Dispatch") ) {
		case TInst(c, _):
			var c = c.get();
			c.meta.remove("getParams");
			c.meta.add("getParams",[{ expr : EConst(CString(str)), pos : c.pos }],c.pos);
		default:
		}
	}

	static function serialize( v : Dynamic ) {
		var s = new haxe.Serializer();
		s.useEnumIndex = true;
		s.serialize(v);
		return s.toString();
	}

	public static function getRunParam(i:Int) {
		return PARAMS[i].t;
	}

	public dynamic static function checkMeta( f : ClassField ) {
	}

	#end

	public static macro function make( obj : ExprOf<{}> ) : ExprOf<DispatchConfig> {
		return makeConfig(obj);
	}

	public static macro function run( url : ExprOf<String>, params : ExprOf<haxe.ds.StringMap<String>>, obj : ExprOf<{}> ) : ExprOf<Void> {
		var p = Context.currentPos();
		var cfg = makeConfig(obj);
		return { expr : ECall({ expr : EField({ expr : ENew({ name : "Dispatch", pack : ["haxe","web"], params : [], sub : null },[url,params]), pos : p },"runtimeDispatch"), pos : p },[cfg]), pos : p };
	}

	static function extractConfig( obj : Dynamic ) : DispatchConfig {
		// extract the config from the class metadata (cache result)
		var c = Type.getClass(obj);
		var dc = haxe.rtti.Meta.getType(c);
		var m : Dynamic = dc.dispatchConfig[0];
		if( Std.is(m,String) ) {
			m = haxe.Unserializer.run(m);
			dc.dispatchConfig[0] = m;
		}
		return { obj : obj, rules : m };
	}

}
