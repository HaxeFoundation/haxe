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
 package java.db;
import java.util.concurrent.atomic.AtomicInteger;
import haxe.io.Bytes;
import java.sql.Types;

@:native('haxe.java.db.Jdbc')
class Jdbc
{

	public static function create(cnx:java.sql.Connection):sys.db.Connection
	{
		return new JdbcConnection(cnx);
	}

}

@:native('haxe.java.db.JdbcConnection')
private class JdbcConnection implements sys.db.Connection
{
	private static var ids = new AtomicInteger(0);
	private var id:Int;

	private var cnx:java.sql.Connection;
	private var _lastInsertId:Int;
	//escape handling
	private var escapeRegex:EReg;
	private var escapes:Array<Dynamic>;

	public function new(cnx)
	{
		this.id = ids.getAndIncrement();
		this.cnx = cnx;
		this.escapes = [];
		this.escapeRegex = ~/@@HX_ESCAPE(\d+)_(\d+)@@/;
	}

	public function close()
	{
		try
			this.cnx.close()
		catch(e:Dynamic) throw e;
	}

	public function escape(s:String):String
	{
		return "@@HX_ESCAPE" + id + "_" +escapes.push(s) + "@@";
	}

	public function quote(s:String):String
	{
		return "@@HX_ESCAPE" + id + "_" +escapes.push(s) + "@@";
	}

	public function addValue(s:StringBuf, v:Dynamic)
	{
		if (Std.is(v, Date))
		{
			v = Std.string(v);
		} else if (Std.is(v, Bytes)) {
			var bt:Bytes = v;
			v = bt.getData();
		}
		s.add("@@HX_ESCAPE");
		s.add(id);
		s.add("_");
		s.add(escapes.push(v));
		s.add("@@");
	}

	public function lastInsertId():Int
	{
		return _lastInsertId;
	}

	public function dbName():String
	{
		try {
			var ret = cnx.getMetaData().getDriverName();
			var retc = ret.toLowerCase();
			if (retc.indexOf("mysql") != -1)
				return "MySQL";
			else if (retc.indexOf("sqlite") != -1)
				return "SQLite";
			return ret;
		} catch(e:Dynamic) { throw e; }
	}

	public function startTransaction()
	{
		try
		{
			cnx.setAutoCommit(false);
		}
		catch(e:Dynamic) throw e;
	}

	public function commit()
	{
		try
		{
			cnx.commit();
		}
		catch(e:Dynamic)
		{
			throw e;
		}
	}

	public function rollback()
	{
		try
			cnx.rollback()
		catch(e:Dynamic) throw e;
	}

	public function request(s:String):sys.db.ResultSet
	{
		var newst = new StringBuf();
		var sentArray = [];

		//cycle through the request string, adding any @@HX_ESCAPE@@ reference to the sentArray
		var r = escapeRegex;
		var myid = id + "", escapes = escapes, elen = escapes.length;
		try
		{
			while (r.match(s))
			{
				var id = r.matched(1);
				if (id != myid) throw "Request quotes are only valid for one single request; They can't be cached.";

				newst.add(r.matchedLeft());
				var eid = Std.parseInt(r.matched(2));
				if (eid == null || eid > elen)
					throw "Invalid request quote ID " + eid;
				sentArray.push(escapes[eid - 1]);
				newst.add("?");
				s = r.matchedRight();
			}
			newst.add(s);
			var stmt = cnx.prepareStatement(newst.toString(), java.sql.Statement.Statement_Statics.RETURN_GENERATED_KEYS);
			for (i in 0...sentArray.length)
			{
				stmt.setObject(i + 1, sentArray[i]);
			}

			var ret = null, dbName = dbName();
			if (stmt.execute())
			{
				//is a result set
				var rs = stmt.getResultSet();
				ret = new JdbcResultSet(rs, dbName, stmt.getMetaData());
			} else {
				//is an update
				var affected = stmt.getUpdateCount();
				if (affected == 1)
				{
					var autogen = stmt.getGeneratedKeys();
					if (autogen.next())
					{
						this._lastInsertId = autogen.getInt(1);
					}
				}
				ret = new JdbcResultSet(null, dbName,null);
			}

			if (escapes.length != 0)
				escapes = [];
			this.id = ids.getAndIncrement();
			return ret;
		}
		catch(e:Dynamic)
		{
			if (escapes.length != 0)
				escapes = [];
			this.id = ids.getAndIncrement();
			throw e;
		}
	}

}

@:native('haxe.java.db.JdbcResultSet')
private class JdbcResultSet implements sys.db.ResultSet
{
	@:isVar public var length(get,null) : Int;
	public var nfields(get,null) : Int;

	private var rs:java.sql.ResultSet;
	private var names:Array<String>;
	private var types:java.NativeArray<Int>;
	private var dbName:String;
	private var didNext:Bool;

	public function new(rs, dbName, meta:java.sql.ResultSetMetaData)
	{
		this.dbName = dbName;
		this.rs = rs;
		if (meta != null)
		{
			try {
				var count = meta.getColumnCount();
				var names = [], types = new NativeArray(count);
				for (i in 0...count)
				{
					names.push(meta.getColumnName(i+1));
					types[i] = meta.getColumnType(i+1);
				}
				this.types = types;
				this.names = names;
			} catch(e:Dynamic) throw e;
		}
	}

	private function get_length():Int
	{
		if (length == 0)
		{
      try
      {
  			var cur = rs.getRow();
  			rs.last();
  			this.length = rs.getRow();
  			rs.absolute(cur);
      } catch(e:Dynamic) throw e;
		}
		return length;
	}

	private function get_nfields():Int
	{
		return names == null ? 0 : names.length;
	}

	public function hasNext() : Bool
	{
		try
		{
			didNext = true;
			return rs != null && rs.next();
		}
		catch(e:Dynamic) { return throw e; }
	}

	public function next() : Dynamic
	{
		try {
			if (rs == null) return null;
			if (didNext)
			{
				didNext = false;
			} else {
				if (!rs.next())
				{
					return null;
				}
			}
			var ret = {}, names = names, types = types;
			for (i in 0...names.length)
			{
				var name = names[i], t = types[i], val:Dynamic = null;
				if (t == Types.FLOAT)
				{
					val = rs.getDouble(i+1);
				} else if (t == Types.DATE || t == Types.TIME) {
					if (dbName == "SQLite")
					{
						var str = rs.getString(i+1);
						if (str != null)
						{
							var d:Date = Date.fromString(str);
							val = d;
						}
					} else {
						var d:java.sql.Date = rs.getDate(i+1);
						if (d != null)
							val = Date.fromTime(cast d.getTime());
					}
				} else if (t == Types.LONGVARBINARY || t == Types.VARBINARY || t == Types.BINARY || t == Types.BLOB) {
					var b = rs.getBytes(i+1);
					if (b != null)
						val = Bytes.ofData(b);
				} else {
					untyped __java__("val = rs.getObject(i + 1)"); //type parameter constraint + overloads
				}
				Reflect.setField(ret, name, val);
			}
			return ret;
		} catch(e:Dynamic) throw e;
	}

	public function results() : List<Dynamic>
	{
		var l = new List();
		if (rs == null) return l;

		try
		{
			while(hasNext())
				l.add(next());
		} catch(e:Dynamic) throw e;
		return l;
	}

	public function getResult( n : Int ) : String
	{
    try
    {
  		return rs.getString(n);
    } catch(e:Dynamic) throw e;
	}

	public function getIntResult( n : Int ) : Int
	{
		try
		{
			return rs.getInt(n);
		}
		catch(e:Dynamic) { return throw e; };
	}

	public function getFloatResult( n : Int ) : Float
	{
    try
    {
  		return rs.getFloat(n);
    } catch(e:Dynamic) { return throw e; };
	}

	public function getFieldsNames() : Null<Array<String>>
	{
		return this.names;
	}
}
