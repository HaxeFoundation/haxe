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
package sys.db;
import Reflect;
import sys.db.Connection;
import sys.db.RecordInfos;

/**
	Record Manager : the persistent object database manager. See the tutorial on
	Haxe website to learn how to use Record.
**/
#if !macro @:build(sys.db.RecordMacros.addRtti()) #end
@:deprecated("This class will be removed soon, please install the record-macros library")
class Manager<T : Object> {

	/* ----------------------------- STATICS ------------------------------ */
	public static var cnx(default, set) : Connection;
	public static var lockMode : String;

	private static inline var cache_field = "__cache__";

	private static var object_cache : haxe.ds.StringMap<Object> = new haxe.ds.StringMap();
	private static var init_list : List<Manager<Dynamic>> = new List();

	private static var KEYWORDS = {
		var h = new haxe.ds.StringMap();
		for( k in "ADD|ALL|ALTER|ANALYZE|AND|AS|ASC|ASENSITIVE|BEFORE|BETWEEN|BIGINT|BINARY|BLOB|BOTH|BY|CALL|CASCADE|CASE|CHANGE|CHAR|CHARACTER|CHECK|COLLATE|COLUMN|CONDITION|CONSTRAINT|CONTINUE|CONVERT|CREATE|CROSS|CURRENT_DATE|CURRENT_TIME|CURRENT_TIMESTAMP|CURRENT_USER|CURSOR|DATABASE|DATABASES|DAY_HOUR|DAY_MICROSECOND|DAY_MINUTE|DAY_SECOND|DEC|DECIMAL|DECLARE|DEFAULT|DELAYED|DELETE|DESC|DESCRIBE|DETERMINISTIC|DISTINCT|DISTINCTROW|DIV|DOUBLE|DROP|DUAL|EACH|ELSE|ELSEIF|ENCLOSED|ESCAPED|EXISTS|EXIT|EXPLAIN|FALSE|FETCH|FLOAT|FLOAT4|FLOAT8|FOR|FORCE|FOREIGN|FROM|FULLTEXT|GRANT|GROUP|HAVING|HIGH_PRIORITY|HOUR_MICROSECOND|HOUR_MINUTE|HOUR_SECOND|IF|IGNORE|IN|INDEX|INFILE|INNER|INOUT|INSENSITIVE|INSERT|INT|INT1|INT2|INT3|INT4|INT8|INTEGER|INTERVAL|INTO|IS|ITERATE|JOIN|KEY|KEYS|KILL|LEADING|LEAVE|LEFT|LIKE|LIMIT|LINES|LOAD|LOCALTIME|LOCALTIMESTAMP|LOCK|LONG|LONGBLOB|LONGTEXT|LOOP|LOW_PRIORITY|MATCH|MEDIUMBLOB|MEDIUMINT|MEDIUMTEXT|MIDDLEINT|MINUTE_MICROSECOND|MINUTE_SECOND|MOD|MODIFIES|NATURAL|NOT|NO_WRITE_TO_BINLOG|NULL|NUMERIC|ON|OPTIMIZE|OPTION|OPTIONALLY|OR|ORDER|OUT|OUTER|OUTFILE|PRECISION|PRIMARY|PROCEDURE|PURGE|READ|READS|REAL|REFERENCES|REGEXP|RELEASE|RENAME|REPEAT|REPLACE|REQUIRE|RESTRICT|RETURN|REVOKE|RIGHT|RLIKE|SCHEMA|SCHEMAS|SECOND_MICROSECOND|SELECT|SENSITIVE|SEPARATOR|SET|SHOW|SMALLINT|SONAME|SPATIAL|SPECIFIC|SQL|SQLEXCEPTION|SQLSTATE|SQLWARNING|SQL_BIG_RESULT|SQL_CALC_FOUND_ROWS|SQL_SMALL_RESULT|SSL|STARTING|STRAIGHT_JOIN|TABLE|TERMINATED|THEN|TINYBLOB|TINYINT|TINYTEXT|TO|TRAILING|TRIGGER|TRUE|UNDO|UNION|UNIQUE|UNLOCK|UNSIGNED|UPDATE|USAGE|USE|USING|UTC_DATE|UTC_TIME|UTC_TIMESTAMP|VALUES|VARBINARY|VARCHAR|VARCHARACTER|VARYING|WHEN|WHERE|WHILE|WITH|WRITE|XOR|YEAR_MONTH|ZEROFILL|ASENSITIVE|CALL|CONDITION|CONNECTION|CONTINUE|CURSOR|DECLARE|DETERMINISTIC|EACH|ELSEIF|EXIT|FETCH|GOTO|INOUT|INSENSITIVE|ITERATE|LABEL|LEAVE|LOOP|MODIFIES|OUT|READS|RELEASE|REPEAT|RETURN|SCHEMA|SCHEMAS|SENSITIVE|SPECIFIC|SQL|SQLEXCEPTION|SQLSTATE|SQLWARNING|TRIGGER|UNDO|UPGRADE|WHILE".split("|") )
			h.set(k.toLowerCase(),true);
		h;
	}

	private static function set_cnx( c : Connection ) {
		cnx = c;
		lockMode = (c != null && c.dbName() == "MySQL") ? " FOR UPDATE" : "";
		return c;
	}

	/* ---------------------------- BASIC API ----------------------------- */

	var table_infos : RecordInfos;
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

	public macro function get(ethis,id,?lock:haxe.macro.Expr.ExprOf<Bool>) : #if macro haxe.macro.Expr #else haxe.macro.Expr.ExprOf<T> #end {
		return RecordMacros.macroGet(ethis,id,lock);
	}

	public macro function select(ethis, cond, ?options, ?lock:haxe.macro.Expr.ExprOf<Bool>) : #if macro haxe.macro.Expr #else haxe.macro.Expr.ExprOf<T> #end {
		return RecordMacros.macroSearch(ethis, cond, options, lock, true);
	}

	public macro function search(ethis, cond, ?options, ?lock:haxe.macro.Expr.ExprOf<Bool>) : #if macro haxe.macro.Expr #else haxe.macro.Expr.ExprOf<List<T>> #end {
		return RecordMacros.macroSearch(ethis, cond, options, lock);
	}

	public macro function count(ethis, cond) : #if macro haxe.macro.Expr #else haxe.macro.Expr.ExprOf<Int> #end {
		return RecordMacros.macroCount(ethis, cond);
	}

	public macro function delete(ethis, cond, ?options) : #if macro haxe.macro.Expr #else haxe.macro.Expr.ExprOf<Void> #end {
		return RecordMacros.macroDelete(ethis, cond, options);
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

	/* -------------------------- RECORDOBJECT API -------------------------- */

	function doUpdateCache( x : T, name : String, v : Dynamic ) {
		var cache : { v : Dynamic } = Reflect.field(x, "cache_" + name);
		// if the cache has not been fetched (for instance if the field was set by reflection)
		// then we directly use the new value
		if( cache == null )
			return v;
		var v = doSerialize(name, cache.v);
		// don't set it since the value might change again later
		// Reflect.setField(x, name, v);
		return v;
	}

	static function getFieldName(field:RecordField):String
	{
		return switch (field.t) {
			case DData | DEnum(_):
				"data_" + field.name;
			case _:
				field.name;
		}
	}

	function doInsert( x : T ) {
		unmake(x);
		var s = new StringBuf();
		var fields = new List();
		var values = new List();
		var cache = Reflect.field(x,cache_field);
		if (cache == null)
		{
			Reflect.setField(x,cache_field,cache = {});
		}

		for( f in table_infos.fields ) {
			var name = f.name,
			    fieldName = getFieldName(f);
			var v:Dynamic = Reflect.field(x,fieldName);
			if( v != null ) {
				fields.add(quoteField(name));
				switch( f.t ) {
				case DData: v = doUpdateCache(x, name, v);
				default:
				}
				values.add(v);
			} else if( !f.isNull ) {
				// if the field is not defined, give it a default value on insert
				switch( f.t ) {
				case DUInt, DTinyInt, DInt, DSingle, DFloat, DFlags(_), DBigInt, DTinyUInt, DSmallInt, DSmallUInt, DMediumInt, DMediumUInt, DEnum(_):
					Reflect.setField(x, fieldName, 0);
				case DBool:
					Reflect.setField(x, fieldName, false);
				case DTinyText, DText, DString(_), DSmallText, DSerialized:
					Reflect.setField(x, fieldName, "");
				case DSmallBinary, DNekoSerialized, DLongBinary, DBytes(_), DBinary:
					Reflect.setField(x, fieldName, haxe.io.Bytes.alloc(0));
				case DDate, DDateTime, DTimeStamp:
					// default date might depend on database
				case DId, DUId, DBigId, DNull, DInterval, DEncoded, DData:
					// no default value for these
				}
			}

			Reflect.setField(cache, name, v);
		}
		s.add("INSERT INTO ");
		s.add(table_name);
		if (fields.length > 0 || cnx.dbName() != "SQLite")
		{
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
		} else {
			s.add(" DEFAULT VALUES");
		}
		unsafeExecute(s.toString());
		untyped x._lock = true;
		// table with one key not defined : suppose autoincrement
		if( table_keys.length == 1 && Reflect.field(x,table_keys[0]) == null )
			Reflect.setField(x,table_keys[0],getCnx().lastInsertId());
		addToCache(x);
	}

	inline function isBinary( t : RecordInfos.RecordType ) {
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
		var upd = getUpdateStatement(x);
		if (upd == null) return;
		unsafeExecute(upd);
	}

	function getUpdateStatement( x : T ):Null<String>
	{
		unmake(x);
		var s = new StringBuf();
		s.add("UPDATE ");
		s.add(table_name);
		s.add(" SET ");
		var cache = Reflect.field(x,cache_field);
		var mod = false;
		for( f in table_infos.fields ) {
			if (table_keys.indexOf(f.name) >= 0)
				continue;
			var name = f.name,
			    fieldName = getFieldName(f);
			var v : Dynamic = Reflect.field(x,fieldName);
			var vc : Dynamic = Reflect.field(cache,name);
			if( cache == null || v != vc ) {
				switch( f.t ) {
				case DSmallBinary, DNekoSerialized, DLongBinary, DBytes(_), DBinary:
					if ( !hasBinaryChanged(v,vc) )
						continue;
				case DData:
					v = doUpdateCache(x, name, v);
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
				if ( cache != null )
					Reflect.setField(cache,name,v);
			}
		}
		if( !mod )
			return null;
		s.add(" WHERE ");
		addKeys(s,x);
		return s.toString();
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

	function normalizeCache(x:CacheType<T>)
	{
		for (f in Reflect.fields(x) )
		{
			var val:Dynamic = Reflect.field(x,f), info = table_infos.hfields.get(f);
			if (info != null)
			{
				if (val != null) switch (info.t) {
					case DDate, DDateTime if (!Std.is(val,Date)):
						if (Std.is(val,Float))
						{
							val = Date.fromTime(val);
						} else {
							var v = val + "";
							var index = v.indexOf('.');
							if (index >= 0)
								v = v.substr(0,index);
							val = Date.fromString(v);
						}
					case DSmallBinary, DLongBinary, DBinary, DBytes(_), DData if (Std.is(val, String)):
						val = haxe.io.Bytes.ofString(val);
					case DString(_) | DTinyText | DSmallText | DText if(!Std.is(val,String)):
						val = val + "";
#if (cs && erase_generics)
					// on C#, SQLite Ints are returned as Int64
					case DInt if (!Std.is(val,Int)):
						val = cast(val,Int);
#end
					case DBool if (!Std.is(val,Bool)):
						if (Std.is(val,Int))
							val = val != 0;
						else if (Std.is(val, String)) switch (val.toLowerCase()) {
							case "1", "true": val = true;
							case "0", "false": val = false;
						}
					case DFloat if (Std.is(val,String)):
						val = Std.parseFloat(val);
					case _:
				}
				Reflect.setField(x, f, val);
			}
		}
	}

	function cacheObject( x : T, lock : Bool ) {
		#if neko
		var o = untyped __dollar__new(x);
		untyped __dollar__objsetproto(o, class_proto.prototype);
		#else
		var o : T = Type.createEmptyInstance(cast class_proto);
		untyped o._manager = this;
		#end
		normalizeCache(x);
		for (f in Reflect.fields(x) )
		{
			var val:Dynamic = Reflect.field(x,f), info = table_infos.hfields.get(f);
			if (info != null)
			{
				var fieldName = getFieldName(info);
				Reflect.setField(o, fieldName, val);
			}
		}
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
		var r = unsafeExecute(sql);
		var r = r.hasNext() ? r.next() : null;
		if( r == null )
			return null;
		normalizeCache(r);
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
			normalizeCache(x);
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
		if (a == null || a == 'NULL') {
			return eq ? '$b IS NULL' : '$b IS NOT NULL';
		} else if (b == null || b == 'NULL') {
			return eq ? '$a IS NULL' : '$a IS NOT NULL';
		}
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
			s.add("TRUE");
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
		object_cache = new haxe.ds.StringMap();
	}

	function initRelation( r : RecordInfos.RecordRelation ) {
		// setup getter/setter
		var spod : Dynamic = Type.resolveClass(r.type);
		if( spod == null ) throw "Missing spod type " + r.type;
		var manager : Manager<Dynamic> = spod.manager;
		var hprop = "__"+r.prop;
		var hkey = r.key;
		var lock = r.lock;
		if( manager == null || manager.table_keys == null ) throw ("Invalid manager for relation "+table_name+":"+r.prop);
		if( manager.table_keys.length != 1 ) throw ("Relation " + r.prop + "(" + r.key + ") on a multiple key table");
#if neko
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
#end
	}

	#if !neko

	function __get( x : Dynamic, prop : String, key : String, lock ) {
		var v = Reflect.field(x,prop);
		if( v != null )
			return v;
		var y = unsafeGet(Reflect.field(x, key), lock);
		Reflect.setField(x,prop,v);
		return y;
	}

	function __set( x : Dynamic, prop : String, key : String, v : T ) {
		Reflect.setField(x,prop,v);
		if( v == null )
			Reflect.setField(x,key,null);
		else
			Reflect.setField(x,key,Reflect.field(v,table_keys[0]));
		return v;
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

	function addToCache( x : CacheType<T> ) {
		object_cache.set(makeCacheKey(x),x);
	}

	function removeFromCache( x : CacheType<T> ) {
		object_cache.remove(makeCacheKey(x));
	}

	function getFromCacheKey( key : String ) : T {
		return cast object_cache.get(key);
	}

	function getFromCache( x : CacheType<T>, lock : Bool ) : T {
		var c : Dynamic = object_cache.get(makeCacheKey(x));
		if( c != null && lock && !c._lock ) {
			// synchronize the fields since our result is up-to-date !
			for( f in Reflect.fields(c) )
				Reflect.deleteField(c,f);
			for (f in table_infos.fields)
			{
				var name = f.name,
				    fieldName = getFieldName(f);
				Reflect.setField(c,fieldName,Reflect.field(x,name));
			}
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
		if (v == null) {
			return 'NULL';
		}

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

	// We need Bytes.toString to not be DCE'd. See #1937
	@:keep static function __depends() { return haxe.io.Bytes.alloc(0).toString(); }
}

private typedef CacheType<T> = Dynamic;
