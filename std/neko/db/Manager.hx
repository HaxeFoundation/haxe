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
package neko.db;

import Reflect;

/**
	SPOD Manager : the persistent object database manager. See the tutorial on
	haXe website to learn how to use SPOD.
**/
class Manager<T : Object> {

	/* ----------------------------- STATICS ------------------------------ */
	public static var cnx : Connection = null;
	private static var object_cache : Hash<Object> = new Hash();
	private static var init_list : List<Manager<Object>> = new List();
	private static var cache_field = "__cache__";
	private static var no_update = function() { throw "Cannot update not locked object"; }

	/* ---------------------------- BASIC API ----------------------------- */
	var table_name : String;
	var table_fields : List<String>;
	var table_keys : Array<String>;
	var class_proto : Class;

	public function new( cl : Dynamic ) {
		// get basic infos
		table_name = quoteField(if( cl.TABLE_NAME != null ) cl.TABLE_NAME else cl.__name__.join("_"));
		table_keys = if( cl.TABLE_IDS != null ) cl.TABLE_IDS else ["id"];
		class_proto = cl;

		// get the list of private fields
		var apriv : Array<String> = cl.PRIVATE_FIELDS;
		if( apriv == null ) apriv = new Array();
		apriv.push("local_manager");
		apriv.push("__class__");

		// get the proto fields not marked private (excluding methods)
		table_fields = new List();
		var proto : Dynamic = class_proto.prototype;
		for( f in Reflect.fields(proto) ) {
			var isfield = !Reflect.isFunction(Reflect.field(proto,f));
			if( isfield )
				for( f2 in apriv )
					if( f == f2 ) {
						isfield = false;
						break;
					}
			if( isfield )
				table_fields.add(f);
		}

		// set the manager and ready for further init
		proto.local_manager = this;
		init_list.add(untyped this);
	}

	public function get( id : Int ) : T {
		return getGeneric(id,true);
	}

	public function getReadOnly( id : Int ) : T {
		return getGeneric(id,false);
	}

	public function getWithKeys( keys : {} ) : T {
		var s = new StringBuf();
		s.add("SELECT * FROM ");
		s.add(table_name);
		s.add(" WHERE ");
		addKeys(s,keys);
		s.add(" FOR UPDATE");
		return object(s.toString(),true);
	}

	public function search( x : {}, lock : Bool ) : List<T> {
		var s = new StringBuf();
		s.add("SELECT * FROM ");
		s.add(table_name);
		s.add(" WHERE ");
		var first = true;
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
				addQuote(s,d);
			}
		}
		if( first )
			s.add("TRUE");
		if( lock )
			s.add(" FOR UPDATE");
		return objects(s.toString(),lock);
	}

	public function all(lock) : List<T> {
		return objects("SELECT * FROM "+if( lock ) table_name + " FOR UPDATE" else table_name,lock);
	}

	public function count() : Int {
		return execute("SELECT COUNT(*) FROM "+table_name).getIntResult(0);
	}

	public function quote( s : String ) : String {
		return "'" + cnx.escape( s ) + "'";
	}

	public function result( sql : String ) : Dynamic {
		return cnx.request(sql).next();
	}

	public function results( sql : String ) : List<Dynamic> {
		return cnx.request(sql).results();
	}

	/* -------------------------- SPODOBJECT API -------------------------- */

	public function doInsert( x : T ) {
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
			addQuote(s,v);
		}
		s.add(")");
		execute(s.toString());
		// table with one key not defined : suppose autoincrement
		if( table_keys.length == 1 && Reflect.field(x,table_keys[0]) == null )
			Reflect.setField(x,table_keys[0],lastInsertId());
		addToCache(x);
	}

	public function doUpdate( x : T ) {
		unmake(x);
		var s = new StringBuf();
		s.add("UPDATE ");
		s.add(table_name);
		s.add(" SET ");
		var cache = Reflect.field(x,cache_field);
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
				addQuote(s,v);
				Reflect.setField(cache,f,v);
			}
		}
		if( !mod )
			return;
		s.add(" WHERE ");
		addKeys(s,x);
		execute(s.toString());
	}

	public function doDelete( x : T ) {
		var s = new StringBuf();
		s.add("DELETE FROM ");
		s.add(table_name);
		s.add(" WHERE ");
		addKeys(s,x);
		execute(s.toString());
	}


	public function doSync( i : T ) {
		object_cache.remove(makeCacheKey(i));
		var i2 = getWithKeys(i);
		// set all fields to null
		for( f in Reflect.fields(i) )
			Reflect.setField(i,f,null);
		// copy fields from new object
		for( f in Reflect.fields(i2) )
			Reflect.setField(i,f,Reflect.field(i2,f));
		// set same field-cache
		Reflect.setField(i,cache_field,Reflect.field(i2,cache_field));
		addToCache(i);
	}

	public function objectToString( it : T ) : String {
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
		addToCache(x);
		untyped __dollar__objsetproto(x,class_proto.prototype);
		Reflect.setField(x,cache_field,untyped __dollar__new(x));
		if( !lock )
			x.update = no_update;
	}

	function make( x : T ) {
	}

	function unmake( x : T ) {
	}

	function quoteField(f : String) {
		var fsmall = f.toLowerCase();
		if( fsmall == "read" || fsmall == "desc" || fsmall == "out" || fsmall == "group" )
			return "`"+f+"`";
		return f;
	}

	function addQuote( s : StringBuf, v : Dynamic ) {
		var t = untyped __dollar__typeof(v);
		if( untyped (t == __dollar__tint || t == __dollar__tnull || t == __dollar__tbool) )
			s.add(v);
		else {
			s.add("'");
			s.add(cnx.escape(Std.string(v)));
			s.add("'");
		}
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
			addQuote(s,f);
		}
	}

	function execute( sql : String ) {
		return cnx.request(sql);
	}

	function lastInsertId() : Int {
		return execute("SELECT LAST_INSERT_ID()").getIntResult(0);
	}

	function select( cond : String ) {
		var s = new StringBuf();
		s.add("SELECT * FROM ");
		s.add(table_name);
		s.add(" WHERE ");
		s.add(cond);
		s.add(" FOR UPDATE");
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

	function getGeneric( id : Int, lock : Bool ) : T {
		if( table_keys.length != 1 )
			throw "Invalid number of keys";
		if( id == null )
			return null;
		var x : T = untyped object_cache.get(id + table_name);
		if( x != null )
			return x;
		var s = new StringBuf();
		s.add("SELECT * FROM ");
		s.add(table_name);
		s.add(" WHERE ");
		s.add(quoteField(table_keys[0]));
		s.add(" = ");
		addQuote(s,id);
		if( lock )
			s.add(" FOR UPDATE");
		return object(s.toString(),lock);
	}

	public function object( sql : String, lock : Bool ) : T {
		var r = cnx.request(sql).next();
		if( r == null )
			return null;
		var c = getFromCache(r,lock);
		if( c != null )
			return c;
		cacheObject(r,lock);
		make(r);
		return r;
	}

	public function objects( sql : String, lock : Bool ) : List<T> {
		var me = this;
		var l = cnx.request(sql).results();
		var l2 = new List<T>();
		for( x in l ) {
			var c = getFromCache(x,lock);
			if( c != null )
				l2.add(c);
			else {
				cacheObject(x,lock);
				make(x);
				l2.add(x);
			}
		}
		return l2;
	}

	/* --------------------------- INIT / CLEANUP ------------------------- */

	public static function initialize() {
		var l = init_list;
		init_list = new List();
		for( m in l ) {
			var rl : Void -> Array<Dynamic> = untyped m.class_proto.RELATIONS;
			if( rl != null )
				for( r in rl() )
					m.initRelation(r);
		}
	}

	public static function cleanup() {
		object_cache = new Hash();
	}

	function initRelation(r : { prop : String, key : String, manager : Manager<Object> } ) {
		// setup getter/setter
		var manager = r.manager;
		var hprop = r.prop;
		var hkey = r.key;
		if( manager.table_keys.length != 1 ) throw ("Relation "+r.prop+"("+r.key+") on a multiple key table");
		Reflect.setField(class_proto.prototype,"get_"+r.prop,function() {
			var othis = untyped this;
			var f = Reflect.field(othis,hprop);
			if( f != null )
				return f;
			f = manager.get(Reflect.field(othis,hkey));
			Reflect.setField(othis,hprop,f);
			return f;
		});
		Reflect.setField(class_proto.prototype,"set_"+r.prop,function(f) {
			var othis = untyped this;
			Reflect.setField(othis,hprop,f);
			Reflect.setField(othis,hkey,Reflect.field(f,manager.table_keys[0]));
			return f;
		});
		// remove prop from precomputed table_fields
		// always add key to table fields (even if not declared)
		table_fields.remove(r.prop);
		table_fields.remove(r.key);
		table_fields.add(r.key);
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
		if( c != null && lock && c.update == no_update )
			c.update = class_proto.prototype.update;
		return c;
	}

}
