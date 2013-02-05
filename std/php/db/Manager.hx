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
package php.db;

#if !old_spod

#else

import Reflect;
import php.db.Connection;

/**
	Record Manager : the persistent object database manager. See the tutorial on
	haXe website to learn how to use Record.
**/
class Manager<T : Object> {

	/* ----------------------------- STATICS ------------------------------ */
	public static var cnx(default,set) : Connection;
	private static var object_cache : haxe.ds.StringMap<Object> = new haxe.ds.StringMap();
	private static var cache_field = "__cache__";
	private static var FOR_UPDATE = "";

	public static var managers = new haxe.ds.StringMap<Manager<Dynamic>>();

	private static dynamic function set_cnx( c : Connection ) {
		Reflect.setField(Manager,"cnx",c);
		if( c != null )
			FOR_UPDATE = if( c.dbName() == "MySQL" ) " FOR UPDATE" else "";
		return c;
	}

	/* ---------------------------- BASIC API ----------------------------- */
	var table_name : String;
	var table_fields : List<String>;
	var table_keys : Array<String>;
	var cls : Dynamic; //Class<php.db.Object>;

	public function new( classval : Class<php.db.Object> ) {
		cls = classval;
		var clname = Type.getClassName(cls);
		// get basic infos
		table_name = quoteField((cls.TABLE_NAME != null ) ? cls.TABLE_NAME : clname.split('.').pop());
		table_keys = if( cls.TABLE_IDS != null ) cls.TABLE_IDS else ["id"];

		// get the list of private fields
		var apriv : Array<String> = cls.PRIVATE_FIELDS;
		apriv = if( apriv == null ) new Array() else apriv.copy();
		apriv.push("__cache__");
		apriv.push("__noupdate__");
		apriv.push("__manager__");
		apriv.push("update");

		// get the proto fields not marked private (excluding methods)
		table_fields = new List();
		var stub = Type.createEmptyInstance(cls);

		var instance_fields = Type.getInstanceFields(cls);
		var scls = Type.getSuperClass(cls);
		while(scls != null) {
			for(remove in Type.getInstanceFields(scls))
				instance_fields.remove(remove);
			scls = Type.getSuperClass(scls);
		}

		for( f in instance_fields ) {
			var isfield = !Reflect.isFunction(Reflect.field(stub,f));
			if( isfield )
				for( f2 in apriv ) {
					if(f == f2 ) {
						isfield = false;
						break;
					}
				}
			if( isfield ) {
				table_fields.add(f);
			}
		}

		// set the manager and ready for further init
		managers.set(clname, this);

		var rl : Array<Dynamic>;
		try {
		  rl = untyped cls.RELATIONS();
		} catch(e : Dynamic) { return; }
		for(r in rl) {
			// remove prop from precomputed table_fields
			// always add key to table fields (even if not declared)
			table_fields.remove(r.prop);
			table_fields.remove("get_" + r.prop);
			table_fields.remove("set_" + r.prop);
			table_fields.remove(r.key);
			table_fields.add(r.key);
		}
	}

	public function get( id : Int, ?lock : Bool ) : T {
		if( lock == null )
			lock = true;
		if( table_keys.length != 1 )
			throw "Invalid number of keys";
		if( id == null )
			return null;
		var x : Dynamic = untyped object_cache.get(id + table_name);
		if( x != null && (!lock || !x.__noupdate__) )
			return x;
		var s = new StringBuf();
		s.add("SELECT * FROM ");
		s.add(table_name);
		s.add(" WHERE ");
		s.add(quoteField(table_keys[0]));
		s.add(" = ");
		cnx.addValue(s,id);
		if( lock )
			s.add(FOR_UPDATE);
		return object(s.toString(),lock);
	}

	public function getWithKeys( keys : {}, ?lock : Bool ) : T {
		if( lock == null )
			lock = true;
		var x : Dynamic = getFromCache(untyped keys,false);
		if( x != null && (!lock || !x.__noupdate__) )
			return x;
		var s = new StringBuf();
		s.add("SELECT * FROM ");
		s.add(table_name);
		s.add(" WHERE ");
		addKeys(s,keys);
		if( lock )
			s.add(FOR_UPDATE);
		return object(s.toString(),lock);
	}

	public function delete( x : {} ) {
		var s = new StringBuf();
		s.add("DELETE FROM ");
		s.add(table_name);
		s.add(" WHERE ");
		addCondition(s,x);
		execute(s.toString());
	}

	public function search( x : {}, ?lock : Bool ) : List<T> {
		if( lock == null )
			lock = true;
		var s = new StringBuf();
		s.add("SELECT * FROM ");
		s.add(table_name);
		s.add(" WHERE ");
		addCondition(s,x);
		if( lock )
			s.add(FOR_UPDATE);
		return objects(s.toString(),lock);
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
					cnx.addValue(s,d);
				}
			}
		if( first )
			s.add("1");
	}

	public function all( ?lock: Bool ) : List<T> {
		if( lock == null )
			lock = true;
		return objects("SELECT * FROM " + table_name + if( lock ) FOR_UPDATE else "",lock);
	}

	public function count( ?x : {} ) : Int {
		var s = new StringBuf();
		s.add("SELECT COUNT(*) FROM ");
		s.add(table_name);
		s.add(" WHERE ");
		addCondition(s,x);
		return execute(s.toString()).getIntResult(0);
	}

	public function quote( s : String ) : String {
		return cnx.quote( s );
	}

	public function result( sql : String ) : Dynamic {
		return cnx.request(sql).next();
	}

	public function results<T>( sql : String ) : List<T> {
		return cast cnx.request(sql).results();
	}

	/* -------------------------- RECORDOBJECT API -------------------------- */

	function doInsert( x : T ) {
		unmake(x);
		var s = new StringBuf();
		var fields = new List();
		var values = new List();
		for( f in table_fields ) {
			var v = Reflect.field(x,f);
			if( v != null ) {
				fields.add(quoteField(f));
				values.add(v);
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
			cnx.addValue(s,v);
		}
		s.add(")");
		execute(s.toString());
		// table with one key not defined : suppose autoincrement
		if( table_keys.length == 1 && Reflect.field(x,table_keys[0]) == null )
			Reflect.setField(x,table_keys[0],cnx.lastInsertId());
		addToCache(x);
	}

	function doUpdate( x : T ) {
		unmake(x);
		var s = new StringBuf();
		s.add("UPDATE ");
		s.add(table_name);
		s.add(" SET ");
		var cache = Reflect.field(x, cache_field);
		if (null == cache)
		{
			cache = cacheObject(x, false);
			Reflect.setField(x, cache_field, cache);
		}
		var mod = false;
		for( f in table_fields ) {
			var v = Reflect.field(x,f);
			var vc = Reflect.field(cache,f);
			if( v != vc ) {
				if( mod )
					s.add(", ");
				else
					mod = true;
				s.add(quoteField(f));
				s.add(" = ");
				cnx.addValue(s,v);
				Reflect.setField(cache,f,v);
			}
		}
		if( !mod )
			return;
		s.add(" WHERE ");
		addKeys(s,x);
		execute(s.toString());
	}

	function doDelete( x : T ) {
		var s = new StringBuf();
		s.add("DELETE FROM ");
		s.add(table_name);
		s.add(" WHERE ");
		addKeys(s,x);
		execute(s.toString());
	}


	function doSync( i : T ) {
		object_cache.remove(makeCacheKey(i));
		var i2 = getWithKeys(i, untyped !i.__noupdate__);
		// delete all fields
		for( f in Reflect.fields(i) )
			Reflect.deleteField(i,f);
		// copy fields from new object
		for( f in Reflect.fields(i2) )
			Reflect.setField(i,f,Reflect.field(i2,f));
		// set same field-cache
		Reflect.setField(i,cache_field,Reflect.field(i2,cache_field));
		addToCache(i);
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

	/* ---------------------------- INTERNAL API -------------------------- */

	function cacheObject( x : T, lock : Bool ) {
		var o : T = Type.createEmptyInstance(cls);
		for(field in Reflect.fields(x)) {
			Reflect.setField(o, field, Reflect.field(x, field));
		}
		untyped o.__init_object();
		addToCache(o);
		Reflect.setField(o, cache_field, Type.createEmptyInstance(cls));
		if( !lock )
			untyped o.__noupdate__ = true;
		return o;
	}

	function make( x : T ) {
	}

	function unmake( x : T ) {
	}

	function quoteField(f : String) {
		var fsmall = f.toLowerCase();
		if( fsmall == "read" || fsmall == "desc" || fsmall == "out" || fsmall == "group" || fsmall == "version" || fsmall == "option" )
			return "`"+f+"`";
		return f;
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
			cnx.addValue(s,f);
		}
	}

	function execute( sql : String ) {
		return cnx.request(sql);
	}

	function select( cond : String ) {
		var s = new StringBuf();
		s.add("SELECT * FROM ");
		s.add(table_name);
		s.add(" WHERE ");
		s.add(cond);
		s.add(FOR_UPDATE);
		return s.toString();
	}

	function selectReadOnly( cond : String ) {
		var s = new StringBuf();
		s.add("SELECT * FROM ");
		s.add(table_name);
		s.add(" WHERE ");
		s.add(cond);
		return s.toString();
	}

	public function object( sql : String, lock : Bool ) : T {
		var r = cnx.request(sql).next();
		if( r == null )
			return null;
		var c = getFromCache(r,lock);
		if( c != null )
			return c;
		var o = cacheObject(r,lock);
		make(o);
		return o;
	}

	public function objects( sql : String, lock : Bool ) : List<T> {
		var me = this;
		var l = cnx.request(sql).results();
		var l2 = new List<T>();
		for( x in l ) {
			var c = getFromCache(x,lock);
			if( c != null ) {
				l2.add(c);
			} else {
				var o = cacheObject(x,lock);
				make(o);
				l2.add(o);
			}
		}
		return l2;
	}

	public function dbClass() : Class<Dynamic> {
		return cls;
	}

	/* --------------------------- INIT / CLEANUP ------------------------- */

	/**
	* Left for compability with neko Record
	*/
	public static function initialize() {

	}

	public static function cleanup() {
		object_cache = new haxe.ds.StringMap();
	}

	function initRelation(o : Dynamic, r : { prop : String, key : String, manager : Manager<Object>, lock : Bool } ) {
		// setup getter/setter
		var manager = r.manager;
		var hkey = r.key;
		var lock = r.lock;
		if( lock == null ) lock = true;
		if( manager == null || manager.table_keys == null ) throw ("Invalid manager for relation "+table_name+":"+r.prop);
		if( manager.table_keys.length != 1 ) throw ("Relation "+r.prop+"("+r.key+") on a multiple key table");
		Reflect.setField(o,"get_"+r.prop,function() {
			return manager.get(Reflect.field(o,hkey), lock);
		});
		Reflect.setField(o,"set_"+r.prop,function(f) {
			Reflect.setField(o, hkey, Reflect.field(f, manager.table_keys[0]));
			return f;
		});
	}

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

	function getFromCache( x : T, lock : Bool ) : T {
		var c : Dynamic = object_cache.get(makeCacheKey(x));
		// restore update method since now the object is locked
		if( c != null && lock && c.__noupdate__)
			c.__noupdate__ = false;
		return c;
	}
}
#end