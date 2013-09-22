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
package sys.db;
import sys.db.RecordInfos;
import haxe.macro.Expr;
import haxe.macro.Type.VarAccess;
#if macro
import haxe.macro.Context;
#end

private typedef SqlFunction = {
	var name : String;
	var params : Array<RecordType>;
	var ret : RecordType;
	var sql : String;
}

class RecordMacros {

	static var GLOBAL = null;
	static var simpleString = ~/^[A-Za-z0-9 ]*$/;

	var isNull : Bool;
	var manager : Expr;
	var inf : RecordInfos;
	var g : {
		var cache : haxe.ds.StringMap<RecordInfos>;
		var types : haxe.ds.StringMap<RecordType>;
		var functions : haxe.ds.StringMap<SqlFunction>;
	};

	function new(c) {
		if( GLOBAL != null )
			g = GLOBAL;
		else {
			g = initGlobals();
			GLOBAL = g;
		}
		inf = getRecordInfos(c);
	}

	function initGlobals()
	{
		var cache = new haxe.ds.StringMap();
		var types = new haxe.ds.StringMap();
		for( c in Type.getEnumConstructs(RecordType) ) {
			var e : Dynamic = Reflect.field(RecordType, c);
			if( Std.is(e, RecordType) )
				types.set("S"+c.substr(1), e);
		}
		types.remove("SNull");
		var functions = new haxe.ds.StringMap();
		for( f in [
			{ name : "now", params : [], ret : DDateTime, sql : "NOW($)" },
			{ name : "curDate", params : [], ret : DDate, sql : "CURDATE($)" },
			{ name : "seconds", params : [DFloat], ret : DInterval, sql : "INTERVAL $ SECOND" },
			{ name : "minutes", params : [DFloat], ret : DInterval, sql : "INTERVAL $ MINUTE" },
			{ name : "hours", params : [DFloat], ret : DInterval, sql : "INTERVAL $ HOUR" },
			{ name : "days", params : [DFloat], ret : DInterval, sql : "INTERVAL $ DAY" },
			{ name : "months", params : [DFloat], ret : DInterval, sql : "INTERVAL $ MONTH" },
			{ name : "years", params : [DFloat], ret : DInterval, sql : "INTERVAL $ YEAR" },
			{ name : "date", params : [DDateTime], ret : DDate, sql : "DATE($)" },
		])
			functions.set(f.name, f);
		return { cache : cache, types : types, functions : functions };
	}

	public dynamic function error( msg : String, pos : Position ) : Dynamic {
		#if macro
		Context.error(msg, pos);
		#else
		throw msg;
		#end
		return null;
	}

	public dynamic function typeof( e : Expr ) : haxe.macro.Type {
		#if macro
		return Context.typeof(e);
		#else
		throw "not implemented";
		return null;
		#end
	}

	public dynamic function follow( t : haxe.macro.Type, ?once ) : haxe.macro.Type {
		#if macro
		return Context.follow(t,once);
		#else
		throw "not implemented";
		return null;
		#end
	}

	public dynamic function getManager( t : haxe.macro.Type, p : Position ) : RecordMacros {
		#if macro
		return getManagerInfos(t, p);
		#else
		throw "not implemented";
		return null;
		#end
	}

	public dynamic function resolveType( name : String ) : haxe.macro.Type {
		#if macro
		return Context.getType(name);
		#else
		throw "not implemented";
		return null;
		#end
	}

	function makeInt( t : haxe.macro.Type ) {
		switch( t ) {
		case TInst(c, _):
			var name = c.toString();
			if( name.charAt(0) == "I" )
				return Std.parseInt(name.substr(1));
		default:
		}
		throw "Unsupported " + Std.string(t);
	}

	function makeRecord( t : haxe.macro.Type ) {
		switch( t ) {
		case TInst(c, _):
			var name = c.toString();
			var cl = c.get();
			var csup = cl.superClass;
			while( csup != null ) {
				if( csup.t.toString() == "sys.db.Object" )
					return name;
				csup = csup.t.get().superClass;
			}
		case TType(t, p):
			var name = t.toString();
			if( p.length == 1 && (name == "Null" || name == "sys.db.SNull") ) {
				isNull = true;
				return makeRecord(p[0]);
			}
		default:
		}
		return null;
	}

	function getFlags( t : haxe.macro.Type ) {
		switch( t ) {
		case TEnum(e,_):
			var cl = e.get().names;
			if( cl.length > 1 ) {
				var prefix = cl[0];
				for( c in cl )
					while( prefix.length > 0 && c.substr(0, prefix.length) != prefix )
						prefix = prefix.substr(0, -1);
				for( i in 0...cl.length )
					cl[i] = cl[i].substr(prefix.length);
			}
			if( cl.length > 31 ) throw "Too many flags";
			return cl;
		default:
			throw "Flags parameter should be an enum";
		}
	}

	function makeType( t : haxe.macro.Type ) {
		switch( t ) {
		case TInst(c, _):
			var name = c.toString();
			return switch( name ) {
			case "Int": DInt;
			case "Float": DFloat;
			case "String": DText;
			case "Date": DDateTime;
			case "haxe.io.Bytes": DBinary;
			default: throw "Unsupported Record Type " + name;
			}
		case TAbstract(a, _):
			var name = a.toString();
			return switch( name ) {
			case "Int": DInt;
			case "Float": DFloat;
			case "Bool": DBool;
			default: throw "Unsupported Record Type " + name;
			}
		case TEnum(e, _):
			var name = e.toString();
			return switch( name ) {
			case "Bool": DBool;
			default:
				throw "Unsupported Record Type " + name + " (enums must be wrapped with SData or SEnum)";
			}
		case TType(td, p):
			var name = td.toString();
			if( StringTools.startsWith(name, "sys.db.") )
				name = name.substr(7);
			var k = g.types.get(name);
			if( k != null ) return k;
			if( p.length == 1 )
				switch( name ) {
				case "SString": return DString(makeInt(p[0]));
				case "SBytes": return DBytes(makeInt(p[0]));
				case "SNull", "Null": isNull = true; return makeType(p[0]);
				case "SFlags": return DFlags(getFlags(p[0]),false);
				case "SSmallFlags": return DFlags(getFlags(p[0]),true);
				case "SData": return DData;
				case "SEnum":
					switch( p[0] ) {
					case TEnum(en, _):
						var e = en.get();
						var count = 0, hasParam = false;
						for( c in e.constructs ) {
							count++;
							switch( c.type ) {
							case TFun(_):
								hasParam = true;
							default:
							}
						}
						if( hasParam )
							throw "You can't use SEnum if the enum have parameters, try SData instead";
						if( count >= 256 )
							throw "Too many enum constructors";
						return DEnum(en.toString());
					default:
						// should cause another error
					}
				default:
				}
			return makeType(follow(t, true));
		default:
		}
		throw "Unsupported Record Type " + Std.string(t);
	}

	function makeIdent( e : Expr ) {
		return switch( e.expr ) {
		case EConst(c):
			switch( c ) {
			case CIdent(s): s;
			default: error("Identifier expected", e.pos);
			}
		default: error("Identifier expected", e.pos);
		}
	}

	function getRecordInfos( c : haxe.macro.Type.Ref<haxe.macro.Type.ClassType> ) : RecordInfos {
		var cname = c.toString();
		var i = g.cache.get(cname);
		if( i != null ) return i;
		i = {
			key : null,
			name : cname.split(".").pop(), // remove package name
			fields : [],
			hfields : new haxe.ds.StringMap(),
			relations : [],
			indexes : [],
		};
		var c = c.get();
		var fieldsPos = new haxe.ds.StringMap();
		var fields = c.fields.get();
		var csup = c.superClass;
		while( csup != null ) {
			var c = csup.t.get();
			if( !c.meta.has(":skipFields") )
				fields = c.fields.get().concat(fields);
			csup = c.superClass;
		}
		for( f in fields ) {
			fieldsPos.set(f.name, f.pos);
			switch( f.kind ) {
			case FMethod(_):
				// skip methods
				continue;
			case FVar(g, s):
				// skip not-db fields
				if( f.meta.has(":skip") )
					continue;
				// handle relations
				if( f.meta.has(":relation") ) {
					if( !Type.enumEq(g,AccCall) || !Type.enumEq(s,AccCall) )
						error("Relation should be (dynamic,dynamic)", f.pos);
					for( m in f.meta.get() ) {
						if( m.name != ":relation" ) continue;
						if( m.params.length == 0 ) error("Missing relation key", m.pos);
						var params = [];
						for( p in m.params )
							params.push({ i : makeIdent(p), p : p.pos });
						isNull = false;
						var t = makeRecord(f.type);
						if( t == null ) error("Relation type should be a sys.db.Object", f.pos);
						var r = {
							prop : f.name,
							key : params.shift().i,
							type : t,
							cascade : false,
							lock : false,
							isNull : isNull,
						};
						// setup flags
						for( p in params )
							switch( p.i ) {
							case "lock": r.lock = true;
							case "cascade": r.cascade = true;
							default: error("Unknown relation flag", p.p);
							}
						i.relations.push(r);
					}
					continue;
				}
				switch( g ) {
				case AccCall:
					if( !f.meta.has(":data") )
						error("Relation should be defined with @:relation(key)", f.pos);
				default:
				}
			}
			isNull = false;
			var fi = {
				name : f.name,
				t : try makeType(f.type) catch( e : String ) error(e,f.pos),
				isNull : isNull,
			};
			var isId = switch( fi.t ) {
			case DId, DUId: true;
			default: fi.name == "id";
			}
			if( isId ) {
				if( i.key == null ) i.key = [fi.name] else error("Multiple table id declaration", f.pos);
			}
			i.fields.push(fi);
			i.hfields.set(fi.name, fi);
		}
		// create fields for undeclared relations keys :
		for( r in i.relations ) {
			var f = i.hfields.get(r.key);
			if( f == null ) {
				f = {
					name : r.key,
					t : DInt,
					isNull : r.isNull,
				};
				i.fields.push(f);
				i.hfields.set(f.name, f);
			} else {
				var pos = fieldsPos.get(f.name);
				if( f.t != DInt ) error("Relation key should be SInt", pos);
				if( f.isNull != r.isNull ) error("Relation and field should have same nullability", pos);
			}
		}
		// process class metadata
		for( m in c.meta.get() )
			switch( m.name ) {
			case ":id":
				i.key = [];
				for( p in m.params ) {
					var id = makeIdent(p);
					if( !i.hfields.exists(id) )
						error("This field does not exists", p.pos);
					i.key.push(id);
				}
				if( i.key.length == 0 ) error("Invalid :id", m.pos);
			case ":index":
				var idx = [];
				for( p in m.params ) idx.push(makeIdent(p));
				var unique = idx[idx.length - 1] == "unique";
				if( unique ) idx.pop();
				if( idx.length == 0 ) error("Invalid :index", m.pos);
				for( k in 0...idx.length )
					if( !i.hfields.exists(idx[k]) )
						error("This field does not exists", m.params[k].pos);
				i.indexes.push( { keys : idx, unique : unique } );
			case ":table":
				if( m.params.length != 1 ) error("Invalid :table", m.pos);
				i.name = switch( m.params[0].expr ) {
				case EConst(c): switch( c ) { case CString(s): s; default: null; }
				default: null;
				};
				if( i.name == null ) error("Invalid :table value", m.params[0].pos);
			default:
			}
		// check primary key defined
		if( i.key == null )
			error("Table is missing unique id, use either SId or @:id", c.pos);
		g.cache.set(cname, i);
		return i;
	}

	function quoteField( f : String ) {
		var m : { private var KEYWORDS : haxe.ds.StringMap<Bool>; } = Manager;
		return m.KEYWORDS.exists(f.toLowerCase()) ? "`"+f+"`" : f;
	}

	function initManager( pos : Position ) {
		manager = { expr : EField({ expr : EField({ expr : EConst(CIdent("sys")), pos : pos },"db"), pos : pos }, "Manager"), pos : pos };
	}

	inline function makeString( s : String, pos ) {
		return { expr : EConst(CString(s)), pos : pos };
	}

	inline function makeOp( op : String, e1, e2, pos ) {
		return sqlAdd(sqlAddString(e1,op),e2,pos);
	}

	inline function sqlAdd( e1 : Expr, e2 : Expr, pos : Position ) {
		return { expr : EBinop(OpAdd, e1, e2), pos : pos };
	}

	inline function sqlAddString( sql : Expr, s : String ) {
		return { expr : EBinop(OpAdd, sql, makeString(s,sql.pos)), pos : sql.pos };
	}

	function sqlQuoteValue( v : Expr, t : RecordType ) {
		switch( v.expr ) {
		case EConst(c):
			switch( c ) {
			case CInt(_), CFloat(_): return v;
			case CString(s):
				if( simpleString.match(s) ) return { expr : EConst(CString("'"+s+"'")), pos : v.pos };
			case CIdent(n):
				switch( n ) {
				case "null": return { expr : EConst(CString("NULL")), pos : v.pos };
				case "true": return { expr : EConst(CInt("1")), pos : v.pos };
				case "false": return { expr : EConst(CInt("0")), pos : v.pos };
				}
			default:
			}
		default:
		}
		return { expr : ECall( { expr : EField(manager, "quoteAny"), pos : v.pos }, [ensureType(v,t)]), pos : v.pos }
	}

	inline function sqlAddValue( sql : Expr, v : Expr, t : RecordType ) {
		return { expr : EBinop(OpAdd, sql, sqlQuoteValue(v,t)), pos : sql.pos };
	}

	function unifyClass( t : RecordType ) {
		return switch( t ) {
		case DId, DInt, DUId, DUInt, DEncoded, DFlags(_), DTinyInt, DTinyUInt, DSmallInt, DSmallUInt, DMediumInt, DMediumUInt: 0;
		case DBigId, DBigInt, DSingle, DFloat: 1;
		case DBool: 2;
		case DString(_), DTinyText, DSmallText, DText, DSerialized: 3;
		case DDate, DDateTime, DTimeStamp: 4;
		case DSmallBinary, DLongBinary, DBinary, DBytes(_), DNekoSerialized, DData: 5;
		case DInterval: 6;
		case DNull: 7;
		case DEnum(_): -1;
		};
	}

	function tryUnify( t, rt ) {
		if( t == rt ) return true;
		var c = unifyClass(t);
		if( c < 0 ) return Type.enumEq(t, rt);
		var rc = unifyClass(rt);
		return c == rc || (c == 0 && rc == 1); // allow Int-to-Float expansion
	}

	function typeStr( t : RecordType ) {
		return Std.string(t).substr(1);
	}

	function canStringify( t : RecordType ) {
		return switch( unifyClass(t) ) {
		case 0, 1, 2, 3, 4, 5, 7: true;
		default: false;
		};
	}

	function convertType( t : RecordType ) {
		var pack = [];
		return TPath( {
			name : switch( unifyClass(t) ) {
			case 0: "Int";
			case 1: "Float";
			case 2: "Bool";
			case 3: "String";
			case 4: "Date";
			case 5: pack = ["haxe", "io"];  "Bytes";
			default: throw "assert";
			},
			pack : pack,
			params : [],
			sub : null,
		});
	}

	function unify( t : RecordType, rt : RecordType, pos : Position ) {
		if( !tryUnify(t, rt) )
			error(typeStr(t) + " should be " + typeStr(rt), pos);
	}

	function buildCmp( op, e1, e2, pos ) {
		var r1 = buildCond(e1);
		var r2 = buildCond(e2);
		unify(r2.t, r1.t, e2.pos);
		if( !tryUnify(r1.t, DFloat) && !tryUnify(r1.t, DDate) && !tryUnify(r1.t, DText) )
			unify(r1.t, DFloat, e1.pos);
		return { sql : makeOp(op, r1.sql, r2.sql, pos), t : DBool, n : r1.n || r2.n };
	}

	function buildNum( op, e1, e2, pos ) {
		var r1 = buildCond(e1);
		var r2 = buildCond(e2);
		var c1 = unifyClass(r1.t);
		var c2 = unifyClass(r2.t);
		if( c1 > 1 ) {
			if( op == "-" && tryUnify(r1.t, DDateTime) && tryUnify(r2.t,DInterval) )
				return { sql : makeOp(op, r1.sql, r2.sql, pos), t : DDateTime, n : r1.n };
			unify(r1.t, DInt, e1.pos);
		}
		if( c2 > 1 ) unify(r2.t, DInt, e2.pos);
		return { sql : makeOp(op, r1.sql, r2.sql, pos), t : (c1 + c2) == 0 ? DInt : DFloat, n : r1.n || r2.n };
	}

	function buildInt( op, e1, e2, pos ) {
		var r1 = buildCond(e1);
		var r2 = buildCond(e2);
		unify(r1.t, DInt, e1.pos);
		unify(r2.t, DInt, e2.pos);
		return { sql : makeOp(op, r1.sql, r2.sql, pos), t : DInt, n : r1.n || r2.n };
	}

	function buildEq( eq, e1 : Expr, e2, pos ) {
		var r1 = null;
		switch( e1.expr ) {
		case EConst(c):
			switch( c ) {
			case CIdent(i):
				if( i.charCodeAt(0) == "$".code ) {
					var tmp = { field : i.substr(1), expr : e2 };
					var f = getField(tmp);
					r1 = { sql : makeString(quoteField(tmp.field), e1.pos), t : f.t, n : f.isNull };
					e2 = tmp.expr;
					switch( f.t ) {
					case DEnum(e):
						var ok = false;
						switch( e2.expr ) {
						case EConst(c):
							switch( c ) {
							case CIdent(n):
								if( n.charCodeAt(0) == '$'.code )
									ok = true;
								else switch( resolveType(e) ) {
								case TEnum(e, _):
									var c = e.get().constructs.get(n);
									if( c == null ) {
										if( n == "null" )
											return { sql : sqlAddString(r1.sql, eq ? " IS NULL" : " IS NOT NULL"), t : DBool, n : false };
										error("Unknown constructor " + n, e2.pos);
									} else {
										return { sql : makeOp(eq?" = ":" != ", r1.sql, { expr : EConst(CInt(Std.string(c.index))), pos : e2.pos }, pos), t : DBool, n : r1.n };
									}
								default:
								}
							default:
							}
						default:
						}
						if( !ok )
							error("Should be a constant constructor", e2.pos);
					default:
					}
				}
			default:
			}
		default:
		}
		if( r1 == null )
			r1 = buildCond(e1);
		var r2 = buildCond(e2);
		if( r2.t == DNull ) {
			if( !r1.n )
				error("Expression can't be null", e1.pos);
			return { sql : sqlAddString(r1.sql, eq ? " IS NULL" : " IS NOT NULL"), t : DBool, n : false };
		} else {
			unify(r2.t, r1.t, e2.pos);
			unify(r1.t, r2.t, e1.pos);
		}
		var sql;
		// use some different operators if there is a possibility for comparing two NULLs
		if( r1.n || r2.n )
			sql = { expr : ECall({ expr : EField(manager,"nullCompare"), pos : pos },[r1.sql,r2.sql,{ expr : EConst(CIdent(eq?"true":"false")), pos : pos }]), pos : pos };
		else
			sql = makeOp(eq?" = ":" != ", r1.sql, r2.sql, pos);
		return { sql : sql, t : DBool, n : r1.n || r2.n };
	}

	function buildDefault( cond : Expr ) {
		var t = typeof(cond);
		isNull = false;
		var d = try makeType(t) catch( e : String ) try makeType(follow(t)) catch( e : String ) error("Unsupported type " + Std.string(t), cond.pos);
		return { sql : sqlQuoteValue(cond, d), t : d, n : isNull };
	}

	function getField( f : { field : String, expr : Expr } ) {
		var fi = inf.hfields.get(f.field);
		if( fi == null ) {
			for( r in inf.relations )
				if( r.prop == f.field ) {
					var path = r.type.split(".");
					var p = f.expr.pos;
					path.push("manager");
					var first = path.shift();
					var mpath = { expr : EConst(CIdent(first)), pos : p };
					for ( e in path )
						mpath = { expr : EField(mpath, e), pos : p };
					var m = getManager(typeof(mpath),p);
					var getid = { expr : ECall( { expr : EField(mpath, "unsafeGetId"), pos : p }, [f.expr]), pos : p };
					f.field = r.key;
					f.expr = ensureType(getid, m.inf.hfields.get(m.inf.key[0]).t);
					return inf.hfields.get(r.key);
				}
			error("No database field '" + f.field+"'", f.expr.pos);
		}
		return fi;
	}

	function buildCond( cond : Expr ) {
		var sql = null;
		var p = cond.pos;
		switch( cond.expr ) {
		case EObjectDecl(fl):
			var first = true;
			var sql = makeString("(", p);
			var fields = new haxe.ds.StringMap();
			for( f in fl ) {
				var fi = getField(f);
				if( first )
					first = false;
				else
					sql = sqlAddString(sql, " AND ");
				sql = sqlAddString(sql, quoteField(fi.name) + (fi.isNull ? " <=> " : " = "));
				sql = sqlAddValue(sql, f.expr, fi.t);
				if( fields.exists(fi.name) )
					error("Duplicate field " + fi.name, p);
				else
					fields.set(fi.name, true);
			}
			if( first ) sqlAddString(sql, "TRUE");
			sql = sqlAddString(sql, ")");
			return { sql : sql, t : DBool, n : false };
		case EParenthesis(e):
			var r = buildCond(e);
			r.sql = sqlAdd(makeString("(", p), r.sql, p);
			r.sql = sqlAddString(r.sql, ")");
			return r;
		case EBinop(op, e1, e2):
			switch( op ) {
			case OpAdd:
				var r1 = buildCond(e1);
				var r2 = buildCond(e2);
				var rt = if( tryUnify(r1.t, DFloat) && tryUnify(r2.t, DFloat) )
					tryUnify(r1.t, DInt) ? tryUnify(r2.t, DInt) ? DInt : DFloat : DFloat;
				else if( (tryUnify(r1.t, DText) && canStringify(r2.t)) || (tryUnify(r2.t, DText) && canStringify(r1.t)) )
					return { sql : sqlAddString(sqlAdd(sqlAddString(sqlAdd(makeString("CONCAT(",p),r1.sql,p),","),r2.sql,p),")"), t : DText, n : r1.n || r2.n }
				else
					error("Can't add " + typeStr(r1.t) + " and " + typeStr(r2.t), p);
				return { sql : makeOp("+", r1.sql, r2.sql, p), t : rt, n : r1.n || r2.n };
			case OpBoolAnd, OpBoolOr:
				var r1 = buildCond(e1);
				var r2 = buildCond(e2);
				unify(r1.t, DBool, e1.pos);
				unify(r2.t, DBool, e2.pos);
				return { sql : makeOp(op == OpBoolAnd ? " AND " : " OR ", r1.sql, r2.sql, p), t : DBool, n : false };
			case OpGte:
				return buildCmp(">=", e1, e2, p);
			case OpLte:
				return buildCmp("<=", e1, e2, p);
			case OpGt:
				return buildCmp(">", e1, e2, p);
			case OpLt:
				return buildCmp("<", e1, e2, p);
			case OpSub:
				return buildNum("-", e1, e2, p);
			case OpDiv:
				var r = buildNum("/", e1, e2, p);
				r.t = DFloat;
				return r;
			case OpMult:
				return buildNum("*", e1, e2, p);
			case OpEq, OpNotEq:
				return buildEq(op == OpEq, e1, e2, p);
			case OpXor:
				return buildInt("^", e1, e2, p);
			case OpOr:
				return buildInt("|", e1, e2, p);
			case OpAnd:
				return buildInt("&", e1, e2, p);
			case OpShr:
				return buildInt(">>", e1, e2, p);
			case OpShl:
				return buildInt("<<", e1, e2, p);
			case OpMod:
				return buildNum("%", e1, e2, p);
			case OpUShr, OpInterval, OpAssignOp(_), OpAssign, OpArrow:
				error("Unsupported operation", p);
			}
		case EUnop(op, _, e):
			var r = buildCond(e);
			switch( op ) {
			case OpNot:
				var sql = makeString("!", p);
				unify(r.t, DBool, e.pos);
				switch( r.sql.expr ) {
				case EConst(_):
				default:
					r.sql = sqlAddString(r.sql, ")");
					sql = sqlAddString(sql, "(");
				}
				return { sql : sqlAdd(sql, r.sql, p), t : DBool, n : r.n };
			case OpNegBits:
				var sql = makeString("~", p);
				unify(r.t, DInt, e.pos);
				return { sql : sqlAdd(sql, r.sql, p), t : DInt, n : r.n };
			case OpNeg:
				var sql = makeString("-", p);
				unify(r.t, DFloat, e.pos);
				return { sql : sqlAdd(sql, r.sql, p), t : r.t, n : r.n };
			case OpIncrement, OpDecrement:
				error("Unsupported operation", p);
			}
		case EConst(c):
			switch( c ) {
			case CInt(s): return { sql : makeString(s, p), t : DInt, n : false };
			case CFloat(s): return { sql : makeString(s, p), t : DFloat, n : false };
			case CString(s): return { sql : sqlQuoteValue(cond, DText), t : DString(s.length), n : false };
			case CRegexp(_): error("Unsupported", p);
			case CIdent(n):
				if( n.charCodeAt(0) == "$".code ) {
					n = n.substr(1);
					var f = inf.hfields.get(n);
					if( f == null ) error("Unknown database field '" + n + "'", p);
					return { sql : makeString(quoteField(f.name), p), t : f.t, n : f.isNull };
				}
				switch( n ) {
				case "null":
					return { sql : makeString("NULL", p), t : DNull, n : true };
				case "true":
					return { sql : makeString("1", p), t : DBool, n : false };
				case "false":
					return { sql : makeString("0", p), t : DBool, n : false };
				}
				return buildDefault(cond);
			}
		case ECall(c, pl):
			switch( c.expr ) {
			case EConst(co):
				switch(co) {
				case CIdent(t):
					if( t.charCodeAt(0) == '$'.code ) {
						var f = g.functions.get(t.substr(1));
						if( f == null ) error("Unknown method " + t, c.pos);
						if( f.params.length != pl.length ) error("Function " + f.name + " requires " + f.params.length + " parameters", p);
						var parts = f.sql.split("$");
						var sql = makeString(parts[0], p);
						var first = true;
						var isNull = false;
						for( i in 0...f.params.length ) {
							var r = buildCond(pl[i]);
							if( r.n ) isNull = true;
							unify(r.t, f.params[i], pl[i].pos);
							if( first )
								first = false;
							else
								sql = sqlAddString(sql, ",");
							sql = sqlAdd(sql, r.sql, p);
						}
						sql = sqlAddString(sql, parts[1]);
						// assume that for all SQL functions, a NULL parameter will make a NULL result
						return { sql : sql, t : f.ret, n : isNull };
					}
				default:
				}
			case EField(e, f):
				switch( f ) {
				case "like":
					if( pl.length == 1 ) {
						var r = buildCond(e);
						var v = buildCond(pl[0]);
						if( !tryUnify(r.t, DText) ) {
							if( tryUnify(r.t, DBinary) )
								unify(v.t, DBinary, pl[0].pos);
							else
								unify(r.t, DText, e.pos);
						} else
							unify(v.t, DText, pl[0].pos);
						return { sql : makeOp(" LIKE ", r.sql, v.sql, p), t : DBool, n : r.n || v.n };
					}
				case "has":
					if( pl.length == 1 ) {
						var r = buildCond(e);
						switch( r.t ) {
						case DFlags(vals,_):
							var id = makeIdent(pl[0]);
							var idx = Lambda.indexOf(vals,id);
							if( idx < 0 ) error("Flag should be "+vals.join(","), pl[0].pos);
							return { sql : sqlAddString(r.sql, " & " + (1 << idx) + " != 0"), t : DBool, n : r.n };
						default:
						}
					}
				}
			default:
			}
			return buildDefault(cond);
		case EField(_, _), EDisplay(_):
			return buildDefault(cond);
		case EIf(e, e1, e2), ETernary(e, e1, e2):
			if( e2 == null ) error("If must have an else statement", p);
			var r1 = buildCond(e1);
			var r2 = buildCond(e2);
			unify(r2.t, r1.t, e2.pos);
			unify(r1.t, r2.t, e1.pos);
			return { sql : { expr : EIf(e, r1.sql, r2.sql), pos : p }, t : r1.t, n : r1.n || r2.n };
		case EIn(e, v):
			var e = buildCond(e);
			var t = TPath({
				pack : [],
				name : "Iterable",
				params : [TPType(convertType(e.t))],
				sub : null,
			});
			return { sql : { expr : ECall( { expr : EField(manager, "quoteList"), pos : p }, [e.sql, { expr : ECheckType(v,t), pos : p } ]), pos : p }, t : DBool, n : e.n };
		default:
			return buildDefault(cond);
		}
		error("Unsupported expression", p);
		return null;
	}

	function ensureType( e : Expr, rt : RecordType ) {
		return { expr : ECheckType(e, convertType(rt)), pos : e.pos };
	}

	function checkKeys( econd : Expr ) {
		var p = econd.pos;
		switch( econd.expr ) {
		case EObjectDecl(fl):
			var key = inf.key.copy();
			for( f in fl ) {
				var fi = getField(f);
				if( !key.remove(fi.name) ) {
					if( Lambda.has(inf.key, fi.name) )
						error("Duplicate field " + fi.name, p);
					else
						error("Field " + f.field + " is not part of table key (" + inf.key.join(",") + ")", p);
				}
				f.expr = ensureType(f.expr, fi.t);
			}
			return econd;
		default:
			if( inf.key.length > 1 )
				error("You can't use a single value on a table with multiple keys (" + inf.key.join(",") + ")", p);
			var fi = inf.hfields.get(inf.key[0]);
			return ensureType(econd, fi.t);
		}
	}

	function orderField(e) {
		switch( e.expr ) {
		case EConst(c):
			switch( c ) {
			case CIdent(t):
				if( !inf.hfields.exists(t) )
					error("Unknown database field", e.pos);
				return quoteField(t);
			default:
			}
		case EUnop(op, _, e):
			if( op == OpNeg )
				return orderField(e) + " DESC";
		default:
		}
		error("Invalid order field", e.pos);
		return null;
	}

	function concatStrings( e : Expr ) {
		var inf = { e : null, str : null };
		browseStrings(inf, e);
		if( inf.str != null ) {
			var es = { expr : EConst(CString(inf.str)), pos : e.pos };
			if( inf.e == null )
				inf.e = es;
			else
				inf.e = { expr : EBinop(OpAdd, inf.e, es), pos : e.pos };
		}
		return inf.e;
	}

	function browseStrings( inf : { e : Expr, str : String }, e : Expr ) {
		switch( e.expr ) {
		case EConst(c):
			switch( c ) {
			case CString(s):
				if( inf.str == null )
					inf.str = s;
				else
					inf.str += s;
				return;
			case CInt(s), CFloat(s):
				if( inf.str != null ) {
					inf.str += s;
					return;
				}
			default:
			}
		case EBinop(op, e1, e2):
			if( op == OpAdd ) {
				browseStrings(inf,e1);
				browseStrings(inf,e2);
				return;
			}
		case EIf(cond, e1, e2):
			e = { expr : EIf(cond, concatStrings(e1), concatStrings(e2)), pos : e.pos };
		default:
		}
		if( inf.str != null ) {
			e = { expr : EBinop(OpAdd, { expr : EConst(CString(inf.str)), pos : e.pos }, e), pos : e.pos };
			inf.str = null;
		}
		if( inf.e == null )
			inf.e = e;
		else
			inf.e = { expr : EBinop(OpAdd, inf.e, e), pos : e.pos };
	}

	function buildOptions( eopt : Expr ) {
		var p = eopt.pos;
		var opts = new haxe.ds.StringMap();
		var opt = { limit : null, orderBy : null, forceIndex : null };
		switch( eopt.expr ) {
		case EObjectDecl(fields):
			var limit = null;
			for( o in fields ) {
				if( opts.exists(o.field) ) error("Duplicate option " + o.field, p);
				opts.set(o.field, true);
				switch( o.field ) {
				case "orderBy":
					var fields = switch( o.expr.expr ) {
					case EArrayDecl(vl): Lambda.array(Lambda.map(vl, orderField));
					case ECall(v, pl):
						if( pl.length != 0 || !Type.enumEq(v.expr, EConst(CIdent("rand"))) )
							[orderField(o.expr)]
						else
							["RAND()"];
					default: [orderField(o.expr)];
					};
					opt.orderBy = fields.join(",");
				case "limit":
					var limits = switch( o.expr.expr ) {
					case EArrayDecl(vl): Lambda.array(Lambda.map(vl, buildDefault));
					default: [buildDefault(o.expr)];
					}
					if( limits.length == 0 || limits.length > 2 ) error("Invalid limits", o.expr.pos);
					var l0 = limits[0], l1 = limits[1];
					unify(l0.t, DInt, l0.sql.pos);
					if( l1 != null )
						unify(l1.t, DInt, l1.sql.pos);
					opt.limit = { pos : l0.sql, len : l1 == null ? null : l1.sql };
				case "forceIndex":
					var fields = switch( o.expr.expr ) {
					case EArrayDecl(vl): Lambda.array(Lambda.map(vl, makeIdent));
					default: [makeIdent(o.expr)];
					}
					for( f in fields )
						if( !inf.hfields.exists(f) )
							error("Unknown field " + f, o.expr.pos);
					var idx = fields.join(",");
					if( !Lambda.exists(inf.indexes, function(i) return i.keys.join(",") == idx) && !Lambda.exists(inf.relations,function(r) return r.key == idx) )
						error("These fields are not indexed", o.expr.pos);
					opt.forceIndex = idx;
				default:
					error("Unknown option '" + o.field + "'", p);
				}
			}
		default:
			error("Options should be { orderBy : field, limit : [a,b] }", p);
		}
		return opt;
	}

	public static function getInfos( t : haxe.macro.Type ) {
		var c = switch( t ) {
		case TInst(c, _): if( c.toString() == "sys.db.Object" ) return null; c;
		default: return null;
		};
		return new RecordMacros(c);
	}


	#if macro
	static var RTTI = false;
	static var FIRST_COMPILATION = true;

	public static function addRtti() : Array<Field> {
		if( RTTI ) return null;
		RTTI = true;
		if( FIRST_COMPILATION ) {
			FIRST_COMPILATION = false;
			Context.onMacroContextReused(function() {
				RTTI = false;
				GLOBAL = null;
				return true;
			});
		}
		Context.getType("sys.db.RecordInfos");
		Context.onGenerate(function(types) {
			for( t in types )
				switch( t ) {
				case TInst(c, _):
					var c = c.get();
					var cur = c.superClass;
					while( cur != null ) {
						if( cur.t.toString() == "sys.db.Object" )
							break;
						cur = cur.t.get().superClass;
					}
					if( cur == null || c.meta.has(":skip") || c.meta.has("rtti") )
						continue;
					var inst = getInfos(t);
					var s = new haxe.Serializer();
					s.useEnumIndex = true;
					s.useCache = true;
					s.serialize(inst.inf);
					c.meta.add("rtti", [ { expr : EConst(CString(s.toString())), pos : c.pos } ], c.pos);
				default:
				}
		});
		Context.registerModuleReuseCall("sys.db.Manager", "sys.db.RecordMacros.addRtti()");
		return null;
	}

	static function getManagerInfos( t : haxe.macro.Type, pos ) {
		var param = null;
		switch( t ) {
		case TInst(c, p):
			while( true ) {
				if( c.toString() == "sys.db.Manager" ) {
					param = p[0];
					break;
				}
				var csup = c.get().superClass;
				if( csup == null ) break;
				c = csup.t;
				p = csup.params;
			}
		case TType(t, p):
			if( p.length == 1 && t.toString() == "sys.db.Manager" )
				param = p[0];
		default:
		}
		var inst = if( param == null ) null else getInfos(param);
		if( inst == null )
			Context.error("This method must be called from a specific Manager", Context.currentPos());
		inst.initManager(pos);
		return inst;
	}

	static function buildSQL( em : Expr, econd : Expr, prefix : String, ?eopt : Expr ) {
		var pos = Context.currentPos();
		var inst = getManagerInfos(Context.typeof(em),pos);
		var sql = { expr : EConst(CString(prefix + " " + inst.quoteField(inst.inf.name))), pos : econd.pos };
		var r = inst.buildCond(econd);
		if( r.t != DBool ) Context.error("Expression should be a condition", econd.pos);
		if( eopt != null && !Type.enumEq(eopt.expr, EConst(CIdent("null"))) ) {
			var opt = inst.buildOptions(eopt);
			if( opt.orderBy != null )
				r.sql = inst.sqlAddString(r.sql, " ORDER BY " + opt.orderBy);
			if( opt.limit != null ) {
				r.sql = inst.sqlAddString(r.sql, " LIMIT ");
				r.sql = inst.sqlAdd(r.sql, opt.limit.pos, pos);
				if( opt.limit.len != null ) {
					r.sql = inst.sqlAddString(r.sql, ",");
					r.sql = inst.sqlAdd(r.sql, opt.limit.len, pos);
				}
			}
			if( opt.forceIndex != null )
				sql = inst.sqlAddString(sql, " FORCE INDEX (" + inst.inf.name+"_"+opt.forceIndex+")");
		}
		sql = inst.sqlAddString(sql, " WHERE ");
		var sql = inst.sqlAdd(sql, r.sql, sql.pos);
		#if !display
		sql = inst.concatStrings(sql);
		#end
		return sql;
	}

	public static function macroGet( em : Expr, econd : Expr, elock : Expr ) {
		var pos = Context.currentPos();
		var inst = getManagerInfos(Context.typeof(em),pos);
		econd = inst.checkKeys(econd);
		switch( econd.expr ) {
		case EObjectDecl(_):
			return { expr : ECall({ expr : EField(em,"unsafeGetWithKeys"), pos : pos },[econd,elock]), pos : pos };
		default:
			return { expr : ECall({ expr : EField(em,"unsafeGet"), pos : pos },[econd,elock]), pos : pos };
		}
	}

	public static function macroSearch( em : Expr, econd : Expr, eopt : Expr, elock : Expr, ?single ) {
		// allow both search(e,opts) and search(e,lock)
		if( eopt != null && (elock == null || Type.enumEq(elock.expr, EConst(CIdent("null")))) ) {
			switch( eopt.expr ) {
			case EObjectDecl(_):
			default:
				var tmp = eopt;
				eopt = elock;
				elock = tmp;
			}
		}
		var sql = buildSQL(em, econd, "SELECT * FROM", eopt);
		var pos = Context.currentPos();
		var e = { expr : ECall( { expr : EField(em, "unsafeObjects"), pos : pos }, [sql,elock]), pos : pos };
		if( single )
			e = { expr : ECall( { expr : EField(e, "first"), pos : pos }, []), pos : pos };
		return e;
	}

	public static function macroCount( em : Expr, econd : Expr ) {
		var sql = buildSQL(em, econd, "SELECT COUNT(*) FROM");
		var pos = Context.currentPos();
		return { expr : ECall({ expr : EField(em,"unsafeCount"), pos : pos },[sql]), pos : pos };
	}

	public static function macroDelete( em : Expr, econd : Expr, eopt : Expr ) {
		var sql = buildSQL(em, econd, "DELETE FROM", eopt);
		var pos = Context.currentPos();
		return { expr : ECall({ expr : EField(em,"unsafeDelete"), pos : pos },[sql]), pos : pos };
	}

	static var isNeko = Context.defined("neko");

	static function buildField( f : Field, fields : Array<Field>, ft : ComplexType, rt : ComplexType ) {
		var p = switch( ft ) {
		case TPath(p): p;
		default: return;
		}
		if( p.params.length != 1 )
			return;
		var t = switch( p.params[0] ) {
		case TPExpr(_): return;
		case TPType(t): t;
		};
		var pos = f.pos;
		switch( p.name ) {
		case "SData":
			f.kind = FProp("dynamic", "dynamic", rt, null);
			f.meta.push( { name : ":data", params : [], pos : f.pos } );

			var dataName = "data_" + f.name;
			var meta = [ { name : ":hide", params : [], pos : pos } ];
			var cache = "cache_" + f.name;
			var ecache = { expr : EConst(CIdent(cache)), pos : pos };
			var efield = { expr : EConst(CIdent(dataName)), pos : pos };
			var fname = { expr : EConst(CString(dataName)), pos : pos };
			var get = {
				args : [],
				params : [],
				ret : t,
				// we set efield to an empty object to make sure it will be != from previous value when insert/update is triggered
				expr : macro { if( $ecache == null ) { $ecache = { v : untyped manager.doUnserialize($fname, cast $efield) }; Reflect.setField(this, $fname, { } ); }; return $ecache.v; },
			};
			var set = {
				args : [{ name : "_v", opt : false, type : t, value : null }],
				params : [],
				ret : t,
				expr : macro { if( $ecache == null ) { $ecache = { v : _v }; $efield = cast {}; } else $ecache.v = _v; return _v; },
			};
			fields.push( { name : cache, pos : pos, meta : [meta[0], { name:":skip", params:[], pos:pos } ], access : [APrivate], doc : null, kind : FVar(macro : { v : $t }, null) } );
			fields.push( { name : dataName, pos : pos, meta : [meta[0], { name:":skip", params:[], pos:pos } ], access : [APrivate], doc : null, kind : FVar(macro : Dynamic, null) } );
			fields.push( { name : "get_" + f.name, pos : pos, meta : meta, access : [APrivate], doc : null, kind : FFun(get) } );
			fields.push( { name : "set_" + f.name, pos : pos, meta : meta, access : [APrivate], doc : null, kind : FFun(set) } );
		case "SEnum":
			f.kind = FProp("dynamic", "dynamic", rt, null);
			f.meta.push( { name : ":isVar", params : [], pos : f.pos } );
			f.meta.push( { name : ":data", params : [], pos : f.pos } );
			var meta = [ { name : ":hide", params : [], pos : pos } ];
			var efield = { expr : EConst(CIdent(f.name)), pos : pos };
			var eval = switch( t ) {
			case TPath(p):
				var pack = p.pack.copy();
				pack.push(p.name);
				if( p.sub != null ) pack.push(p.sub);
				Context.parse(pack.join("."), f.pos);
			default:
				Context.error("Enum parameter expected", f.pos);
			}
			var get = {
				args : [],
				params : [],
				ret : t,
				expr : macro return $efield == null ? null : Type.createEnumIndex($eval,cast $efield),
			};
			var set = {
				args : [{ name : "_v", opt : false, type : t, value : null }],
				params : [],
				ret : t,
				expr : macro { $efield = _v == null ? null : cast Type.enumIndex(_v); return _v; },
			};
			fields.push( { name : "get_" + f.name, pos : pos, meta : meta, access : [APrivate], doc : null, kind : FFun(get) } );
			fields.push( { name : "set_" + f.name, pos : pos, meta : meta, access : [APrivate], doc : null, kind : FFun(set) } );
		case "SNull", "Null":
			buildField(f, fields, t, rt);
		}
	}

	public static function macroBuild() {
		var fields = Context.getBuildFields();
		var hasManager = false;
		for( f in fields ) {
			var skip = false;
			if( f.name == "manager") hasManager = true;
			for( m in f.meta )
				switch( m.name ) {
				case ":skip":
					skip = true;
				case ":relation":
					switch( f.kind ) {
					case FVar(t, _):
						f.kind = FProp("dynamic", "dynamic", t);
						if( isNeko )
							continue;
						// create compile-time getter/setter for other platforms
						var relKey = null;
						var relParams = [];
						var lock = false;
						for( p in m.params )
							switch( p.expr ) {
							case EConst(c):
								switch( c ) {
								case CIdent(i):
									relParams.push(i);
								default:
								}
							default:
							}
						relKey = relParams.shift();
						for( p in relParams )
							if( p == "lock" )
								lock = true;
						// we will get an error later
						if( relKey == null )
							continue;
						// generate get/set methods stubs
						var pos = f.pos;
						var ttype = t, tname;
						while( true )
							switch(ttype) {
							case TPath(t):
								if( t.params.length == 1 && (t.name == "Null" || t.name == "SNull") ) {
									ttype = switch( t.params[0] ) {
									case TPType(t): t;
									default: throw "assert";
									};
									continue;
								}
								var p = t.pack.copy();
								p.push(t.name);
								if( t.sub != null ) p.push(t.sub);
								tname = p.join(".");
								break;
							default:
								Context.error("Relation type should be a type path", f.pos);
							}
						function e(expr) return { expr : expr, pos : pos };
						var get = {
							args : [],
							params : [],
							ret : t,
							expr : Context.parse("return untyped "+tname+".manager.__get(this,'"+f.name+"','"+relKey+"',"+lock+")",pos),
						};
						var set = {
							args : [{ name : "_v", opt : false, type : t, value : null }],
							params : [],
							ret : t,
							expr : Context.parse("return untyped "+tname+".manager.__set(this,'"+f.name+"','"+relKey+"',_v)",pos),
						};
						var meta = [{ name : ":hide", params : [], pos : pos }];
						f.meta.push({ name : ":isVar", params : [], pos : pos });
						fields.push({ name : "get_"+f.name, pos : pos, meta : meta, access : [APrivate], doc : null, kind : FFun(get) });
						fields.push({ name : "set_"+f.name, pos : pos, meta : meta, access : [APrivate], doc : null, kind : FFun(set) });
						fields.push({ name : relKey, pos : pos, meta : [{ name : ":skip", params : [], pos : pos }], access : [APrivate], doc : null, kind : FVar(TPath({ sub : null, params : [], pack : [], name : "Dynamic" })) });
					default:
						Context.error("Invalid relation field type", f.pos);
					}
					break;
				default:
				}
			if( skip )
				continue;
			switch( f.kind ) {
			case FVar(t, _):
				if( t != null )
					buildField(f,fields,t,t);
			default:
			}
		}
		if( !hasManager ) {
			var inst = Context.getLocalClass().get();
			if( inst.meta.has(":skip") )
				return fields;
			var p = inst.pos;
			var tinst = TPath( { pack : inst.pack, name : inst.name, sub : null, params : [] } );
			var path = inst.pack.copy().concat([inst.name]).join(".");
			var enew = { expr : ENew( { pack : ["sys", "db"], name : "Manager", sub : null, params : [TPType(tinst)] }, [Context.parse(path, p)]), pos : p }
			fields.push({ name : "manager", meta : [], kind : FVar(null,enew), doc : null, access : [AStatic,APublic], pos : p });
		}
		return fields;
	}

	#end

}
