package java.db;
import java.util.concurrent.atomic.AtomicInteger;
import haxe.io.Bytes;
import java.sql.Types;

@:native('haxe.java.db.Jdbc')
class Jdbc
{

	public static function open(str:String):sys.db.Connection
	{

	}

}

private class JdbcConnection implements sys.db.Connection
{
	private var ids = new AtomicInteger(0);
	private var id:Int;

	private var cnx:java.sql.Connection;
	private var escapes:haxe.ds.WeakHashMap<String,Dynamic>;
	private var _lastInsertId:Int;
	private var transaction:java.sql.Savepoint;
	//escape handling
	private var escapeRegex:EReg = ~/@@HX_ESCAPE(\d+)_(\d+)@@/;
	private var escapes:Array<Dynamic>;

	public function new(cnx)
	{
		this.id = ids.getAndIncrement();
		this.cnx = cnx;
		this.escapes = [];
	}

	public function close()
	{
		try
			this.cnx.close()
		catch(e:Dynamic) throw e;
	}

	public function escape(s:String):String
	{
		return "@@HX_ESCAPE" + id + "_" +escapes.push(s) "@@";
	}

	public function quote(s:String):String
	{
		return "@@HX_ESCAPE" + id + "_" +escapes.push(s) "@@";
	}

	public function addValue(s:StringBuf, v:Dynamic)
	{
		if (Std.is(v, Date))
		{
			var d:Date = v;
			v = new java.sql.Date(cast(d.getTime(), haxe.Int64));
		} else if (Std.is(v, Bytes)) {
			var bt:Bytes = v;
			v = v.getData();
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
		var ret = cnx.getMetaData().getDriverName();
		var retInsens = ret.toLowerCase();
		if (retInses.indexOf("mysql") != -1)
			return "MySQL";
		else if (retInses.indexOf("sqlite") != -1)
			return "SQLite";
		return ret;
	}

	public function startTransaction()
	{
		if (transaction != null) throw "A transaction was already started!";
		try
		{
			transaction = cnx.setSavepoint();
			cnx.setAutoCommit(true);
		}
		catch(e:Dynamic) throw e;
	}

	public function commit()
	{
		try
		{
			cnx.commit();
			transaction = cnx.setSavePoint();
		}
		catch(e:Dynamic)
		{
			throw e;
		}
	}

	public function rollback()
	{
		try
			cnx.rollback(transaction)
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
			var stmt = cnx.prepareStatement(newst.toString());
			for (i in 0...sentArray.length)
			{
				stmt.setObject(i + 1, sentArray[i]);
			}

			var ret = null;
			if (stmt.execute())
			{
				//is a result set
				var rs = stmt.getResultSet()
				ret = new JdbcResultSet(rs, stmt.getMetaData());
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
				ret = new JdbcResultSet(null,null);
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

private class JdbcResultSet implements sys.db.ResultSet
{
	public var length(get,null) : Int;
	public var nfields(get,null) : Int;

	private var rs:java.sql.ResultSet;
	private var names:Array<String>;
	private var types:java.NativeArray<Int>;

	public function new(rs, meta:java.sql.ResultSetMetaData)
	{
		this.rs = rs;
		if (meta != null)
		{
			var count = meta.getColumnCount();
			var names = [], types = new NativeArray(count);
			for (i in 0...count)
			{
				names.push(meta.getColumnName(i+1));
				types[i] = meta.getColumnType(i+1);
			}
			this.types = types;
			this.names = names;
		}
	}

	public function hasNext() : Bool
	{
		return rs != null && rs.next();
	}

	public function next() : Dynamic
	{
		if (rs == null) return null;
		var ret = {}, names = names, types = types;
		for (i in 0...names.length)
		{
			var name = names[i], t = types[i], val:Dynamic;
			if (t == Types.FLOAT)
			{
				val = rs.getDouble(i+1);
			} else if (t == Types.DATE || t == Types.TIME) {
				var d:java.sql.Date = rs.getDate(i+1);
				val = Date.fromTime(cast d.getTime());
			} else if (t == Types.LONGVARBINARY || t == VARBINARY || t == BINARY || t == BLOB) {
				var b = rs.getBytes(i+1);
				val = Bytes.ofData(b);
			}
			Reflect.setField(ret, name, val);
		}
		return ret;
	}

	public function results() : List<Dynamic>
	{
		var l = new List();
		if (rs == null) return l;

		while(rs.next())
			l.add(next());
		return l;
	}

	public function getResult( n : Int ) : String
	{
		return rs.getString(n);
	}

	public function getIntResult( n : Int ) : Int
	{
		return rs.getInt(n);
	}

	public function getFloatResult( n : Int ) : Float
	{
		return rs.getFloat(n);
	}

	public function getFieldsNames() : Null<Array<String>>
	{
		return this.names;
	}
}
