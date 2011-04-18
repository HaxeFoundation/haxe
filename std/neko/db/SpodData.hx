package neko.db;
import neko.db.SpodInfos;
import haxe.macro.Expr;
import haxe.macro.Type.VarAccess;
#if macro
import haxe.macro.Context;
#end

private typedef SqlFunction = {
	var name : String;
	var params : Array<SpodType>;
	var ret : SpodType;
	var sql : String;
}

private enum BuildError {
	EExpr( e : Expr );
}

class SpodData {

	static var inst : SpodData = null;
	static var simpleString = ~/^[A-Za-z0-9 ]*$/;

	var cache : Hash<SpodInfos>;
	var types : Hash<SpodType>;
	var isNull : Bool;
	var manager : Expr;
	var current : SpodInfos;
	var functions : Hash<SqlFunction>;

	function new() {
		cache = new Hash();
		types = new Hash();
		for( c in Type.getEnumConstructs(SpodType) ) {
			var e : Dynamic = Reflect.field(SpodType, c);
			if( Std.is(e, SpodType) )
				types.set("S"+c.substr(1), e);
		}
		types.remove("SNull");
		functions = new Hash();
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
	}

	public dynamic function error( msg : String, pos : Position ) : Dynamic {
		#if macro
		Context.error(msg, pos);
		#else
		throw msg;
		#end
		return null;
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

	function makeSpod( t : haxe.macro.Type ) {
		switch( t ) {
		case TInst(c, p):
			var name = c.toString();
			var cl = c.get();
			if( cl.superClass != null && cl.superClass.t.toString() == "neko.db.Object" )
				return name;
		case TType(t, p):
			var name = t.toString();
			if( p.length == 1 && (name == "Null" || name == "neko.db.SNull" || name == "mt.db.SNull") ) {
				isNull = true;
				return makeSpod(p[0]);
			}
		default:
		}
		return null;
	}

	function makeType( t : haxe.macro.Type ) {
		switch( t ) {
		case TInst(c, p):
			var name = c.toString();
			return switch( name ) {
			case "Int": DInt;
			case "Float": DFloat;
			case "String": DText;
			case "Date": DDateTime;
			case "mt.db.SFlags", "neko.db.SFlags":
				switch( p[0] ) {
				case TEnum(e,_):
					var cl = e.get().names;
					var prefix = cl[0];
					for( c in cl )
						while( prefix.length > 0 && c.substr(0, prefix.length) != prefix )
							prefix = prefix.substr(0, -1);
					for( i in 0...cl.length )
						cl[i] = cl[i].substr(prefix.length);
					return DFlags(cl);
				default:
					throw "Flags parameter should be an enum";
				}
			default: throw "Unsupported " + name;
			}
		case TEnum(e, p):
			var name = e.toString();
			return switch( name ) {
			case "Bool": DBool;
			default: throw "Unsupported " + name;
			}
		case TType(t, p):
			var name = t.toString();
			if( StringTools.startsWith(name, "neko.db.") )
				name = name.substr(8);
			else if( StringTools.startsWith(name, "mt.db.") )
				name = name.substr(6);
			var k = types.get(name);
			if( k != null ) return k;
			if( p.length == 1 )
				switch( name ) {
				case "SString": return DString(makeInt(p[0]));
				case "SNull", "Null": isNull = true; return makeType(p[0]);
				default:
				}
			throw "Unsupported " + name;
		default:
		}
		throw "Unsupported " + Std.string(t);
	}

	function makeIdent( e : Expr ) {
		return switch( e.expr ) {
		case EConst(c):
			switch( c ) {
			case CIdent(s), CType(s): s;
			default: error("Identifier expected", e.pos);
			}
		default: error("Identifier expected", e.pos);
		}
	}

	function getSpodInfos( c : haxe.macro.Type.Ref<haxe.macro.Type.ClassType> ) : SpodInfos {
		var cname = c.toString();
		var i = cache.get(cname);
		if( i != null ) return i;
		i = {
			key : null,
			name : cname.split(".").pop(), // remove package name
			fields : [],
			hfields : new Hash(),
			relations : [],
			indexes : [],
		};
		var c = c.get();
		var fieldsPos = new Hash();
		for( f in c.fields.get() ) {
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
					if( !Type.enumEq(g,AccCall("get_" + f.name)) || !Type.enumEq(s,AccCall("set_" + f.name)) )
						error("Relation should be (dynamic,dynamic)", f.pos);
					for( m in f.meta.get() ) {
						if( m.name != ":relation" ) continue;
						if( m.params.length == 0 ) error("Missing relation key", m.pos);
						var params = [];
						for( p in m.params )
							params.push({ i : makeIdent(p), p : p.pos });
						isNull = false;
						var t = makeSpod(f.type);
						if( t == null ) error("Relation type should be a neko.db.Object", f.pos);
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
				case AccCall(_):
					error("Relation should be defined with @:relation(key)", f.pos);
				default:
				}
			}
			isNull = false;
			var fi = {
				name : f.name,
				t : try makeType(f.type) catch( e : Dynamic ) error(Std.string(e),f.pos),
				isNull : isNull,
			};
			switch( fi.t ) {
			case DId, DUId: if( i.key == null ) i.key = [fi.name] else error("Multiple table id declaration", f.pos);
			default:
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
				if( i.key != null ) error("Multiple unique id", m.pos);
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
		cache.set(cname, i);
		return i;
	}

	function quoteField( f : String ) {
		var m : { private var KEYWORDS : Hash<Bool>; } = Manager;
		return m.KEYWORDS.exists(f.toLowerCase()) ? "`"+f+"`" : f;
	}

	function initManager( pos : Position ) {
		manager = { expr : EType({ expr : EField({ expr : EConst(CIdent("neko")), pos : pos },"db"), pos : pos }, "Manager"), pos : pos };
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

	function sqlQuoteValue( v : Expr, t : SpodType ) {
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
		var meth = switch( t ) {
		case DId, DInt, DUId, DUInt, DEncoded, DFlags(_): "quoteInt";
		case DBigId, DBigInt, DSingle, DFloat: "quoteFloat";
		case DBool: "quoteBool";
		case DString(_), DTinyText, DSmallText, DText, DSerialized: "quoteString";
		case DDate, DDateTime: "quoteDate";
		case DSmallBinary, DLongBinary, DBinary, DBytes(_), DNekoSerialized: "quoteBytes";
		case DInterval, DNull: throw "assert";
		};
		return { expr : ECall( { expr : EField(manager, meth), pos : v.pos }, [v]), pos : v.pos }
	}

	inline function sqlAddValue( sql : Expr, v : Expr, t : SpodType ) {
		return { expr : EBinop(OpAdd, sql, sqlQuoteValue(v,t)), pos : sql.pos };
	}

	function unifyClass( t : SpodType ) {
		return switch( t ) {
		case DId, DInt, DUId, DUInt, DEncoded, DFlags(_): 0;
		case DBigId, DBigInt, DSingle, DFloat: 1;
		case DBool: 2;
		case DString(_), DTinyText, DSmallText, DText, DSerialized: 3;
		case DDate, DDateTime: 4;
		case DSmallBinary, DLongBinary, DBinary, DBytes(_), DNekoSerialized: 5;
		case DInterval: 6;
		case DNull: 7;
		};
	}

	function tryUnify( t, rt ) {
		if( t == rt ) return true;
		var c = unifyClass(t);
		var rc = unifyClass(rt);
		return c == rc || (c == 0 && rc == 1); // allow Int-to-Float expansion
	}

	function typeStr( t : SpodType ) {
		return Std.string(t).substr(1);
	}

	function canStringify( t : SpodType ) {
		return switch( unifyClass(t) ) {
		case 0, 1, 2, 3, 4, 5, 7: true;
		default: false;
		};
	}

	function convertType( t : SpodType ) {
		return TPath( {
			name : switch( unifyClass(t) ) {
			case 0: "Int";
			case 1: "Float";
			case 2: "Bool";
			case 3: "String";
			case 4: "Date";
			case 5: "String";
			default: throw "assert";
			},
			pack : [],
			params : [],
			sub : null,
		});
	}

	function unify( t : SpodType, rt : SpodType, pos : Position ) {
		if( !tryUnify(t, rt) )
			error(typeStr(t) + " should be " + typeStr(rt), pos);
	}

	function buildCmp( op, e1, e2, pos ) {
		var r1 = buildCond(e1);
		var r2 = buildCond(e2);
		unify(r2.t, r1.t, e2.pos);
		if( !tryUnify(r1.t, DInt) && !tryUnify(r1.t, DDate) && !tryUnify(r1.t, DText) )
			unify(r1.t, DInt, e1.pos);
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

	function buildEq( eq, e1, e2, pos ) {
		var r1 = buildCond(e1);
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
		if( r1.n && r2.n || (!eq && (r1.n || r2.n)) ) {
			sql = makeOp(" <=> ", r1.sql, r2.sql, pos);
			if( !eq )
				sql = sqlAdd(makeString("NOT(", pos), sqlAddString(sql, ")"), pos);
		} else
			sql = makeOp(eq?" = ":" != ", r1.sql, r2.sql, pos);
		return { sql : sql, t : DBool, n : r1.n || r2.n };
	}

	function buildDefault( cond : Expr ) {
		var t = try Context.typeof(cond) catch( e : Dynamic ) throw BuildError.EExpr(cond);
		isNull = false;
		var d = try makeType(t) catch( e : Dynamic ) try makeType(Context.follow(t)) catch( e : Dynamic ) error("Unsupported type " + Std.string(t), cond.pos);
		return { sql : sqlQuoteValue(cond, d), t : d, n : isNull };
	}

	function buildCond( cond : Expr ) {
		var sql = null;
		var p = cond.pos;
		switch( cond.expr ) {
		case EObjectDecl(fl):
			var first = true;
			var sql = makeString("(", p);
			var fields = new Hash();
			for( f in fl ) {
				var fi = current.hfields.get(f.field);
				if( fi == null ) error("No database field " + f.field, p);
				if( first )
					first = false;
				else
					sql = sqlAddString(sql, " AND ");
				sql = sqlAddString(sql, quoteField(fi.name) + " = ");
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
			case OpUShr, OpInterval, OpAssignOp(_), OpAssign:
				error("Unsupported operation", p);
			}
		case EUnop(op, post, e):
			var r = buildCond(e);
			switch( op ) {
			case OpNot:
				var sql = makeString("!", p);
				unify(r.t, DBool, e.pos);
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
			case CIdent(n), CType(n):
				var f = current.hfields.get(n);
				if( f != null ) {
					if( (try Context.typeof(cond) catch( e : Dynamic ) null) != null )
						error("Possible conflict between variable and database field", p);
					return { sql : makeString(f.name, p), t : f.t, n : f.isNull };
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
				case CIdent(t), CType(t):
					var f = functions.get(t);
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
				default:
				}
			case EField(e, f), EType(e, f):
				switch( f ) {
				case "like":
					if( pl.length == 1 ) {
						var r = buildCond(e);
						var v = buildCond(pl[0]);
						unify(r.t, DText, e.pos);
						unify(v.t, DText, pl[0].pos);
						return { sql : makeOp(" LIKE ", r.sql, v.sql, p), t : DBool, n : r.n || v.n };
					}
				}
			default:
			}
			return buildDefault(cond);
		case EField(_, _), EType(_, _):
			return buildDefault(cond);
		case EIf(e, e1, e2), ETernary(e, e1, e2):
			if( e2 == null ) error("If must have an else statement", p);
			var r1 = buildCond(e1);
			var r2 = buildCond(e2);
			unify(r2.t, r1.t, e2.pos);
			unify(r1.t, r2.t, e1.pos);
			return { sql : { expr : EIf(e, r1.sql, r2.sql), pos : p }, t : r1.t, n : r1.n || r2.n };
		default:
		}
		error("Unsupported expression", p);
		return null;
	}

	function ensureType( e : Expr, rt : SpodType ) {
		var t = try Context.typeof(e) catch( _ : Dynamic ) throw BuildError.EExpr(e);
		switch( t ) {
		case TMono:
			// pseudo-cast
			return { expr : EBlock([
				{ expr : EVars([ { name : "__tmp", type : convertType(rt), expr : e } ]), pos : e.pos },
				{ expr : EConst(CIdent("__tmp")), pos : e.pos },
			]), pos : e.pos };
		default:
			var d = try makeType(t) catch( e : Dynamic ) try makeType(Context.follow(t)) catch( e : Dynamic ) throw BuildError.EExpr(sqlQuoteValue(e,rt)); // will produce an error
			unify(d, rt, e.pos);
			return e;
		}
	}

	function checkKeys( econd : Expr ) {
		var p = econd.pos;
		switch( econd.expr ) {
		case EObjectDecl(fl):
			var key = current.key.copy();
			for( f in fl ) {
				var fi = current.hfields.get(f.field);
				if( fi == null ) error("No database field " + f.field, p);
				if( !key.remove(f.field) ) {
					if( Lambda.has(current.key, f.field) )
						error("Duplicate field " + f.field, p);
					else
						error("Field " + f.field + " is not part of table key (" + current.key.join(",") + ")", p);
				}
				f.expr = ensureType(f.expr, fi.t);
			}
		default:
			if( current.key.length > 1 )
				error("You can't use a single value on a table with multiple keys (" + current.key.join(",") + ")", p);
			var fi = current.hfields.get(current.key[0]);
			var t = try Context.typeof(econd) catch( _ : Dynamic ) throw BuildError.EExpr(econd);
			switch( t ) {
			case TMono:

			default:
				var d = try makeType(t) catch( e : Dynamic ) try makeType(Context.follow(t)) catch( e : Dynamic ) throw BuildError.EExpr(sqlQuoteValue(econd, fi.t));
				unify(d, fi.t, p);
			}
		}
	}

	function orderField(e) {
		switch( e.expr ) {
		case EConst(c):
			switch( c ) {
			case CIdent(t), CType(t):
				if( !current.hfields.exists(t) )
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
		var opts = new Hash();
		var p = eopt.pos;
		var sql = makeString("",p);
		switch( eopt.expr ) {
		case EObjectDecl(fields):
			for( o in fields ) {
				if( opts.exists(o.field) ) error("Duplicate option " + o.field, p);
				switch( o.field ) {
				case "orderBy":
					var fields = switch( o.expr.expr ) {
					case EArrayDecl(vl): Lambda.array(Lambda.map(vl, orderField));
					default: [orderField(o.expr)];
					};
					sql = sqlAddString(sql, " ORDER BY " + fields.join(","));
				case "limit":
					var limits = switch( o.expr.expr ) {
					case EArrayDecl(vl): Lambda.array(Lambda.map(vl, buildDefault));
					default: [buildDefault(o.expr)];
					}
					if( limits.length == 0 || limits.length > 2 ) error("Invalid limits", o.expr.pos);
					var l0 = limits[0], l1 = limits[1];
					unify(l0.t, DInt, l0.sql.pos);
					if( l1 != null ) unify(l1.t, DInt, l1.sql.pos);
					sql = sqlAdd(sqlAddString(sql, " LIMIT "), l0.sql, p);
					if( l1 != null )
						sql = sqlAdd(sqlAddString(sql, ","), l1.sql, p);
				default:
					error("Unknown option '" + o.field + "'", p);
				}
			}
		default:
			error("Options should be { orderBy : field, limit : [a,b] }", p);
		}
		return sql;
	}

	public static function getInfos( t : haxe.macro.Type ) {
		var c = switch( t ) {
		case TInst(c, _): if( c.toString() == "neko.db.Object" ) return null; c;
		default: return null;
		};
		var i = inst;
		if( i == null ) {
			i = new SpodData();
			inst = i;
		}
		return i.getSpodInfos(c);
	}


	#if macro
	static var RTTI = false;

	public static function addRtti() {
		var eret = { expr : EBlock([]), pos : Context.currentPos() };
		if( RTTI ) return eret;
		RTTI = true;
		Context.getType("neko.db.SpodInfos");
		Context.onGenerate(function(types) {
			for( t in types )
				switch( t ) {
				case TInst(c, _):
					var c = c.get();
					var cur = c.superClass;
					while( cur != null ) {
						if( cur.t.toString() == "neko.db.Object" )
							break;
						cur = cur.t.get().superClass;
					}
					if( cur == null || c.meta.has(":skip") )
						continue;
					var inf = getInfos(t);
					var s = new haxe.Serializer();
					s.useEnumIndex = true;
					s.useCache = true;
					s.serialize(inf);
					c.meta.add("rtti", [ { expr : EConst(CString(s.toString())), pos : c.pos } ], c.pos);
				default:
				}
		});
		return eret;
	}

	static function getManagerInfos( t : haxe.macro.Type ) {
		var param = null;
		switch( t ) {
		case TInst(c, p):
			while( true ) {
				if( c.toString() == "neko.db.MacroManager" ) {
					param = p[0];
					break;
				}
				var csup = c.get().superClass;
				if( csup == null ) break;
				c = csup.t;
				p = csup.params;
			}
		case TType(t, p):
			if( p.length == 1 && t.toString() == "neko.db.Manager" )
				param = p[0];
		default:
		}
		var inf = if( param == null ) null else getInfos(param);
		if( inf == null )
			Context.error("This method must be called from a specific Manager", Context.currentPos());
		return inf;
	}

	static function buildSQL( em : Expr, econd : Expr, prefix : String, ?eopt : Expr ) {
		var pos = Context.currentPos();
		var inf = getManagerInfos(Context.typeof(em));
		var sql = { expr : EConst(CString(prefix + " " + inst.quoteField(inf.name) + " WHERE ")), pos : econd.pos };
		inst.current = inf;
		inst.initManager(pos);
		var r = try inst.buildCond(econd) catch( e : BuildError ) switch( e ) { case EExpr(e): return e; };
		if( r.t != DBool ) Context.error("Expression should be a condition", econd.pos);
		if( eopt != null && !Type.enumEq(eopt.expr, EConst(CIdent("null"))) )
			r.sql = inst.sqlAdd(r.sql, inst.buildOptions(eopt), eopt.pos);
		var sql = inst.sqlAdd(sql, r.sql, sql.pos);
		#if !display
		sql = inst.concatStrings(sql);
		#end
		return sql;
	}

	public static function macroGet( em : Expr, econd : Expr, elock : Expr ) {
		var pos = Context.currentPos();
		var inf = getManagerInfos(Context.typeof(em));
		inst.current = inf;
		inst.initManager(pos);
		try inst.checkKeys(econd) catch( e : BuildError ) switch( e ) { case EExpr(e): return e; };
		switch( econd.expr ) {
		case EObjectDecl(_):
			return { expr : ECall({ expr : EField(em,"unsafeGetWithKeys"), pos : pos },[econd,elock]), pos : pos };
		default:
			return { expr : ECall({ expr : EField(em,"unsafeGet"), pos : pos },[econd,elock]), pos : pos };
		}
	}

	public static function macroSearch( em : Expr, econd : Expr, eopt : Expr, elock : Expr, ?single ) {
		if( elock == null || Type.enumEq(elock.expr, EConst(CIdent("null"))) ) {
			var tmp = eopt;
			eopt = elock;
			elock = tmp;
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

	public static function macroDelete( em : Expr, econd : Expr ) {
		var sql = buildSQL(em, econd, "DELETE FROM");
		var pos = Context.currentPos();
		return { expr : ECall({ expr : EField(em,"unsafeDelete"), pos : pos },[sql]), pos : pos };
	}

	#end

}