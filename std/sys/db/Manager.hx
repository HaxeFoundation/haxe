/*
 * Copyright (c) 2005, The haXe Project Contributors
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
package sys.db;
import Reflect;
import sys.db.Connection;

#if (!spod_macro && !doc_gen && !macro)
#error "Please use -D spod_macro when using new SPOD version"
#end

/**
	SPOD Manager : the persistent object database manager. See the tutorial on
	haXe website to learn how to use SPOD.
**/
#if !macro @:build(sys.db.SpodMacros.addRtti()) #end
class Manager<T : Object> {

	/* ----------------------------- STATICS ------------------------------ */
	public static var cnx(default, setConnection) : Connection;
	public static var lockMode : String;

	private static inline var cache_field = "__cache__";

	private static var object_cache : Hash<Object> = new Hash();
	private static var init_list : List<Manager<Dynamic>> = new List();

	private static var KEYWORDS = {
		var h = new Hash();
		for( k in "ADD|ALL|ALTER|ANALYZE|AND|AS|ASC|ASENSITIVE|BEFORE|BETWEEN|BIGINT|BINARY|BLOB|BOTH|BY|CALL|CASCADE|CASE|CHANGE|CHAR|CHARACTER|CHECK|COLLATE|COLUMN|CONDITION|CONSTRAINT|CONTINUE|CONVERT|CREATE|CROSS|CURRENT_DATE|CURRENT_TIME|CURRENT_TIMESTAMP|CURRENT_USER|CURSOR|DATABASE|DATABASES|DAY_HOUR|DAY_MICROSECOND|DAY_MINUTE|DAY_SECOND|DEC|DECIMAL|DECLARE|DEFAULT|DELAYED|DELETE|DESC|DESCRIBE|DETERMINISTIC|DISTINCT|DISTINCTROW|DIV|DOUBLE|DROP|DUAL|EACH|ELSE|ELSEIF|ENCLOSED|ESCAPED|EXISTS|EXIT|EXPLAIN|FALSE|FETCH|FLOAT|FLOAT4|FLOAT8|FOR|FORCE|FOREIGN|FROM|FULLTEXT|GRANT|GROUP|HAVING|HIGH_PRIORITY|HOUR_MICROSECOND|HOUR_MINUTE|HOUR_SECOND|IF|IGNORE|IN|INDEX|INFILE|INNER|INOUT|INSENSITIVE|INSERT|INT|INT1|INT2|INT3|INT4|INT8|INTEGER|INTERVAL|INTO|IS|ITERATE|JOIN|KEY|KEYS|KILL|LEADING|LEAVE|LEFT|LIKE|LIMIT|LINES|LOAD|LOCALTIME|LOCALTIMESTAMP|LOCK|LONG|LONGBLOB|LONGTEXT|LOOP|LOW_PRIORITY|MATCH|MEDIUMBLOB|MEDIUMINT|MEDIUMTEXT|MIDDLEINT|MINUTE_MICROSECOND|MINUTE_SECOND|MOD|MODIFIES|NATURAL|NOT|NO_WRITE_TO_BINLOG|NULL|NUMERIC|ON|OPTIMIZE|OPTION|OPTIONALLY|OR|ORDER|OUT|OUTER|OUTFILE|PRECISION|PRIMARY|PROCEDURE|PURGE|READ|READS|REAL|REFERENCES|REGEXP|RELEASE|RENAME|REPEAT|REPLACE|REQUIRE|RESTRICT|RETURN|REVOKE|RIGHT|RLIKE|SCHEMA|SCHEMAS|SECOND_MICROSECOND|SELECT|SENSITIVE|SEPARATOR|SET|SHOW|SMALLINT|SONAME|SPATIAL|SPECIFIC|SQL|SQLEXCEPTION|SQLSTATE|SQLWARNING|SQL_BIG_RESULT|SQL_CALC_FOUND_ROWS|SQL_SMALL_RESULT|SSL|STARTING|STRAIGHT_JOIN|TABLE|TERMINATED|THEN|TINYBLOB|TINYINT|TINYTEXT|TO|TRAILING|TRIGGER|TRUE|UNDO|UNION|UNIQUE|UNLOCK|UNSIGNED|UPDATE|USAGE|USE|USING|UTC_DATE|UTC_TIME|UTC_TIMESTAMP|VALUES|VARBINARY|VARCHAR|VARCHARACTER|VARYING|WHEN|WHERE|WHILE|WITH|WRITE|XOR|YEAR_MONTH|ZEROFILL|ASENSITIVE|CALL|CONDITION|CONNECTION|CONTINUE|CURSOR|DECLARE|DETERMINISTIC|EACH|ELSEIF|EXIT|FETCH|GOTO|INOUT|INSENSITIVE|ITERATE|LABEL|LEAVE|LOOP|MODIFIES|OUT|READS|RELEASE|REPEAT|RETURN|SCHEMA|SCHEMAS|SENSITIVE|SPECIFIC|SQL|SQLEXCEPTION|SQLSTATE|SQLWARNING|TRIGGER|UNDO|UPGRADE|WHILE".split("|") )
			h.set(k.toLowerCase(),true);
		h;
	}

	private static function setConnection( c : Connection ) {
		cnx = c;
		lockMode = (c != null && c.dbName() == "MySQL") ? " FOR UPDATE" : "";
		return c;
	}

	/* ---------------------------- BASIC API ----------------------------- */

	var table_infos : SpodInfos;
	var table_name : String;
	var table_keys : Array<String>;
	var class_proto : { prototype : Dynamic };

	public function new( classval : Class<T> ) {
		var m : Array<Dynamic> = haxe.rtti.Meta.getType(classval).rtti;
		if( m == null ) throw "Missing @rtti for class " + Type.getClassName(classval);
		table_infos = haxe.Unserializer.run(m[0]);
		table_name = quoteField(table_infos.name);
		table_keys = table_infos.key;
		// set the manager and ready for further init
		class_proto = cast classval;
		#if neko
		class_proto.prototype._manager = this;
		init_list.add(this);
		#end
	}

	public function all( ?lock: Bool ) : List<T> {
		return unsafeObjects("SELECT * FROM " + table_name,lock);
	}

	@:macro public function get(ethis,id,?lock:haxe.macro.Expr.ExprOf<Bool>) : #if macro haxe.macro.Expr #else haxe.macro.Expr.ExprOf<T> #end {
		return SpodMacros.macroGet(ethis,id,lock);
	}

	@:macro public function select(ethis, cond, ?options, ?lock:haxe.macro.Expr.ExprOf<Bool>) : #if macro haxe.macro.Expr #else haxe.macro.Expr.ExprOf<T> #end {
		return SpodMacros.macroSearch(ethis, cond, options, lock, true);
	}

	@:macro public function search(ethis, cond, ?options, ?lock:haxe.macro.Expr.ExprOf<Bool>) : #if macro haxe.macro.Expr #else haxe.macro.Expr.ExprOf<List<T>> #end {
		return SpodMacros.macroSearch(ethis, cond, options, lock);
	}

	@:macro public function count(ethis, cond) : #if macro haxe.macro.Expr #else haxe.macro.Expr.ExprOf<Int> #end {
		return SpodMacros.macroCount(ethis, cond);
	}

	@:macro public function delete(ethis, cond, ?options) : #if macro haxe.macro.Expr #else haxe.macro.Expr.ExprOf<Void> #end {
		return SpodMacros.macroDelete(ethis, cond, options);
	}

	public function dynamicSearch( x : {}, ?lock : Bool ) : List<T> {
		var s = new StringBuf();
		s.add("SELECT * FROM ");
		s.add(table_name);
		s.add(" WHERE ");
		addCondition(s,x);
		return unsafeObjects(s.toString(),lock);
	}

	function quote( s : String ) : String {
		return getCnx().quote( s );
	}

	/* -------------------------- SPODOBJECT API -------------------------- */

	function doUpdateCache( x : T, name : String ) {
		var cache : { v : Dynamic } = Reflect.field(x, "cache_" + name);
		var v = doSerialize(name, cache.v);
		// don't set it since the value might change again later
		// Reflect.setField(x, name, v);
		return v;
	}
	
	function doInsert( x : T ) {
		unmake(x);
		var s = new StringBuf();
		var fields = new List();
		var values = new List();
		for( f in table_infos.fields ) {
			var name = f.name;
			var v = Reflect.field(x,name);
			if( v != null ) {
				fields.add(quoteField(name));
				switch( f.t ) {
				case DData: v = doUpdateCache(x, name);
				default:
				}
				values.add(v);
			} else if( !f.isNull ) {
				// if the field is not defined, give it a default value on insert
				switch( f.t ) {
				case DUInt, DTinyInt, DInt, DSingle, DFloat, DFlags(_), DBigInt, DTinyUInt, DSmallInt, DSmallUInt, DMediumInt, DMediumUInt:
					Reflect.setField(x, name, 0);
				case DBool:
					Reflect.setField(x, name, false);
				case DTinyText, DText, DString(_), DSmallText, DSerialized:
					Reflect.setField(x, name, "");
				case DSmallBinary, DNekoSerialized, DLongBinary, DBytes(_), DBinary:
					Reflect.setField(x, name, haxe.io.Bytes.alloc(0));
				case DDate, DDateTime, DTimeStamp:
					// default date might depend on database
				case DId, DUId, DBigId, DNull, DInterval, DEncoded, DData:
					// no default value for these
				}
			}
		}
		s.add("INSERT INTO ");
		s.add(table_name);
		s.add(" (");
		s.add(fields.join(","));
		s.add(") VALUES (");
		var first = true;
		for( v in values ) {
			if( first )
				first = false;
			else
				s.add(", ");
			getCnx().addValue(s,v);
		}
		s.add(")");
		unsafeExecute(s.toString());
		untyped x._lock = true;
		// table with one key not defined : suppose autoincrement
		if( table_keys.length == 1 && Reflect.field(x,table_keys[0]) == null )
			Reflect.setField(x,table_keys[0],getCnx().lastInsertId());
		addToCache(x);
	}

	inline function isBinary( t : SpodInfos.SpodType ) {
		return switch( t ) {
			case DSmallBinary, DNekoSerialized, DLongBinary, DBytes(_), DBinary: true;
			//case DData: true // -- disabled for implementation purposes
			default: false;
		};
	}

	inline function hasBinaryChanged( a : haxe.io.Bytes, b : haxe.io.Bytes ) {
		return a != b && (a == null || b == null || a.compare(b) != 0);
	}

	function doUpdate( x : T ) {
		if( untyped !x._lock )
			throw "Cannot update a not locked object";
		unmake(x);
		var s = new StringBuf();
		s.add("UPDATE ");
		s.add(table_name);
		s.add(" SET ");
		var cache = Reflect.field(x,cache_field);
		var mod = false;
		for( f in table_infos.fields ) {
			var name = f.name;
			var v : Dynamic = Reflect.field(x,name);
			var vc : Dynamic = Reflect.field(cache,name);
			if( v != vc && (!isBinary(f.t) || hasBinaryChanged(v,vc)) ) {
				switch( f.t ) {
				case DData:
					v = doUpdateCache(x, name);
					if( !hasBinaryChanged(v,vc) )
						continue;
				default:
				}
				if( mod )
					s.add(", ");
				else
					mod = true;
				s.add(quoteField(name));
				s.add(" = ");
				getCnx().addValue(s,v);
				Reflect.setField(cache,name,v);
			}
		}
		if( !mod )
			return;
		s.add(" WHERE ");
		addKeys(s,x);
		unsafeExecute(s.toString());
	}

	function doDelete( x : T ) {
		var s = new StringBuf();
		s.add("DELETE FROM ");
		s.add(table_name);
		s.add(" WHERE ");
		addKeys(s,x);
		unsafeExecute(s.toString());
		removeFromCache(x);
	}

	function doLock( i : T ) {
		if( untyped i._lock )
			return;
		var s = new StringBuf();
		s.add("SELECT * FROM ");
		s.add(table_name);
		s.add(" WHERE ");
		addKeys(s, i);
		// will force sync
		if( unsafeObject(s.toString(),true) != i )
			throw "Could not lock object (was deleted ?); try restarting transaction";
	}

	function objectToString( it : T ) : String {
		var s = new StringBuf();
		s.add(table_name);
		if( table_keys.length == 1 ) {
			s.add("#");
			s.add(Reflect.field(it,table_keys[0]));
		} else {
			s.add("(");
			var first = true;
			for( f in table_keys ) {
				if( first )
					first = false;
				else
					s.add(",");
				s.add(quoteField(f));
				s.add(":");
				s.add(Reflect.field(it,f));
			}
			s.add(")");
		}
		return s.toString();
	}

	function doSerialize( field : String, v : Dynamic ) : haxe.io.Bytes {
		var s = new haxe.Serializer();
		s.useEnumIndex = true;
		s.serialize(v);
		var str = s.toString();
		#if neko
		return neko.Lib.bytesReference(str);
		#else
		return haxe.io.Bytes.ofString(str);
		#end
	}
	
	function doUnserialize( field : String, b : haxe.io.Bytes ) : Dynamic {
		if( b == null )
			return null;
		var str;
		#if neko
		str = neko.Lib.stringReference(b);
		#else
		str = b.toString();
		#end
		if( str == "" )
			return null;
		return haxe.Unserializer.run(str);
	}
	
	/* ---------------------------- INTERNAL API -------------------------- */

	function cacheObject( x : T, lock : Bool ) {
		#if neko
		var o = untyped __dollar__new(x);
		untyped __dollar__objsetproto(o, class_proto.prototype);
		#else
		var o : T = Type.createEmptyInstance(cast class_proto);
		for( f in Reflect.fields(x) )
			Reflect.setField(o, f, Reflect.field(x, f));
		untyped o._manager = this;
		#end
		Reflect.setField(o,cache_field,x);
		addToCache(o);
		untyped o._lock = lock;
		return o;
	}

	function make( x : T ) {
	}

	function unmake( x : T ) {
	}

	function quoteField(f : String) {
		return KEYWORDS.exists(f.toLowerCase()) ? "`"+f+"`" : f;
	}

	function addKeys( s : StringBuf, x : {} ) {
		var first = true;
		for( k in table_keys ) {
			if( first )
				first = false;
			else
				s.add(" AND ");
			s.add(quoteField(k));
			s.add(" = ");
			var f = Reflect.field(x,k);
			if( f == null )
				throw ("Missing key "+k);
			getCnx().addValue(s,f);
		}
	}

	function unsafeExecute( sql : String ) {
		return getCnx().request(sql);
	}

	public function unsafeObject( sql : String, lock : Bool ) : T {
		if( lock != false ) {
			lock = true;
			sql += getLockMode();
		}
		var r = unsafeExecute(sql).next();
		if( r == null )
			return null;
		var c = getFromCache(r,lock);
		if( c != null )
			return c;
		r = cacheObject(r,lock);
		make(r);
		return r;
	}

	public function unsafeObjects( sql : String, lock : Bool ) : List<T> {
		if( lock != false ) {
			lock = true;
			sql += getLockMode();
		}
		var l = unsafeExecute(sql).results();
		var l2 = new List<T>();
		for( x in l ) {
			var c = getFromCache(x,lock);
			if( c != null )
				l2.add(c);
			else {
				x = cacheObject(x,lock);
				make(x);
				l2.add(x);
			}
		}
		return l2;
	}

	public function unsafeCount( sql : String ) {
		return unsafeExecute(sql).getIntResult(0);
	}

	public function unsafeDelete( sql : String ) {
		unsafeExecute(sql);
	}

	public function unsafeGet( id : Dynamic, ?lock : Bool ) : T {
		if( lock == null ) lock = true;
		if( table_keys.length != 1 )
			throw "Invalid number of keys";
		if( id == null )
			return null;
		var x : Dynamic = getFromCacheKey(Std.string(id) + table_name);
		if( x != null && (!lock || x._lock) )
			return x;
		var s = new StringBuf();
		s.add("SELECT * FROM ");
		s.add(table_name);
		s.add(" WHERE ");
		s.add(quoteField(table_keys[0]));
		s.add(" = ");
		getCnx().addValue(s,id);
		return unsafeObject(s.toString(), lock);
	}

	public function unsafeGetWithKeys( keys : { }, ?lock : Bool ) : T {
		if( lock == null ) lock = true;
		var x : Dynamic = getFromCacheKey(makeCacheKey(cast keys));
		if( x != null && (!lock || x._lock) )
			return x;
		var s = new StringBuf();
		s.add("SELECT * FROM ");
		s.add(table_name);
		s.add(" WHERE ");
		addKeys(s,keys);
		return unsafeObject(s.toString(),lock);
	}

	public function unsafeGetId( o : T ) : Dynamic {
		return o == null ? null : Reflect.field(o, table_keys[0]);
	}

	public static function nullCompare( a : String, b : String, eq : Bool ) {
		// we can't use a null-safe operator here
		if( cnx.dbName() != "MySQL" )
			return a + (eq ? " = " : " != ") + b;
		var sql = a+" <=> "+b;
		if( !eq ) sql = "NOT("+sql+")";
		return sql;
	}

	function addCondition(s : StringBuf,x) {
		var first = true;
		if( x != null )
			for( f in Reflect.fields(x) ) {
				if( first )
					first = false;
				else
					s.add(" AND ");
				s.add(quoteField(f));
				var d = Reflect.field(x,f);
				if( d == null )
					s.add(" IS NULL");
				else {
					s.add(" = ");
					getCnx().addValue(s,d);
				}
			}
		if( first )
			s.add("1");
	}

	/* --------------------------- MISC API  ------------------------------ */

	public function dbClass() : Class<Dynamic> {
		return cast class_proto;
	}

	public function dbInfos() {
		return table_infos;
	}

	function getCnx() {
		return cnx;
	}

	function getLockMode() {
		return lockMode;
	}

	/**
		Remove the cached value for the given Object field : this will ensure
		that the value is updated when calling .update(). This is necessary if
		you are modifying binary data in-place since the cache will be modified
		as well.
	**/
	public function forceUpdate( o : T, field : String ) {
		// set a reference that will ensure != and .compare() != 0
		Reflect.setField(Reflect.field(o,cache_field),field,null);
	}

	/* --------------------------- INIT / CLEANUP ------------------------- */

	public static function initialize() {
		var l = init_list;
		init_list = new List();
		for( m in l )
			for( r in m.table_infos.relations )
				m.initRelation(r);
	}

	public static function cleanup() {
		object_cache = new Hash();
	}

	function initRelation( r : SpodInfos.SpodRelation ) {
		// setup getter/setter
		var spod : Dynamic = Type.resolveClass(r.type);
		if( spod == null ) throw "Missing spod type " + r.type;
		var manager : Manager<Dynamic> = spod.manager;
		var hprop = "__"+r.prop;
		var hkey = r.key;
		var lock = r.lock;
		if( manager == null || manager.table_keys == null ) throw ("Invalid manager for relation "+table_name+":"+r.prop);
		if( manager.table_keys.length != 1 ) throw ("Relation " + r.prop + "(" + r.key + ") on a multiple key table");
		Reflect.setField(class_proto.prototype,"get_"+r.prop,function() {
			var othis = untyped __this__;
			var f = Reflect.field(othis,hprop);
			if( f != null )
				return f;
			var id = Reflect.field(othis, hkey);
			if( id == null )
				return null;
			f = manager.unsafeGet(id,lock);
			// it's highly possible that in that case the object has been inserted
			// after we started our transaction : in that case, let's lock it, since
			// it's still better than returning 'null' while it exists
			if( f == null && id != null && !lock )
				f = manager.unsafeGet(id,true);
			Reflect.setField(othis,hprop,f);
			return f;
		});
		Reflect.setField(class_proto.prototype,"set_"+r.prop,function(f) {
			var othis = untyped __this__;
			Reflect.setField(othis,hprop,f);
			Reflect.setField(othis,hkey,Reflect.field(f,manager.table_keys[0]));
			return f;
		});
	}

	#if !neko

	function __get( x : Dynamic, prop : String, key : String, lock ) {
		var v = Reflect.field(x,prop);
		if( v != null )
			return v.value;
		var y = unsafeGet(Reflect.field(x, key), lock);
		Reflect.setField(x,prop,{ value : y });
		return y;
	}

	function __set( x : Dynamic, prop : String, key : String, v : T ) {
		Reflect.setField(x,prop,{ value : v });
		if( v == null )
			Reflect.setField(x,key,null);
		else
			Reflect.setField(x,key,Reflect.field(v,table_keys[0]));
	}

	#end

	/* ---------------------------- OBJECT CACHE -------------------------- */

	function makeCacheKey( x : T ) : String {
		if( table_keys.length == 1 ) {
			var k = Reflect.field(x,table_keys[0]);
			if( k == null )
				throw("Missing key "+table_keys[0]);
			return Std.string(k)+table_name;
		}
		var s = new StringBuf();
		for( k in table_keys ) {
			var v = Reflect.field(x,k);
			if( k == null )
				throw("Missing key "+k);
			s.add(v);
			s.add("#");
		}
		s.add(table_name);
		return s.toString();
	}

	function addToCache( x : T ) {
		object_cache.set(makeCacheKey(x),x);
	}

	function removeFromCache( x : T ) {
		object_cache.remove(makeCacheKey(x));
	}

	function getFromCacheKey( key : String ) : T {
		return cast object_cache.get(key);
	}

	function getFromCache( x : T, lock : Bool ) : T {
		var c : Dynamic = object_cache.get(makeCacheKey(x));
		if( c != null && lock && !c._lock ) {
			// synchronize the fields since our result is up-to-date !
			for( f in Reflect.fields(c) )
				Reflect.deleteField(c,f);
			for( f in Reflect.fields(x) )
				Reflect.setField(c,f,Reflect.field(x,f));
			// mark as locked
			c._lock = true;
			// restore our manager
			#if !neko
			untyped c._manager = this;
			#end
			// use the new object as our cache of fields
			Reflect.setField(c,cache_field,x);
			// remake object
			make(c);
		}
		return c;
	}

	/* ---------------------------- QUOTES -------------------------- */

	public static function quoteAny( v : Dynamic ) {
		var s = new StringBuf();
		cnx.addValue(s, v);
		return s.toString();
	}

	public static function quoteList( v : String, it : Iterable<Dynamic> ) {
		var b = new StringBuf();
		var first = true;
		if( it != null )
			for( v in it ) {
				if( first ) first = false else b.addChar(','.code);
				cnx.addValue(b, v);
			}
		if( first )
			return "FALSE";
		return v + " IN (" + b.toString() + ")";
	}

}
