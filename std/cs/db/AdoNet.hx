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
 package cs.db;
import sys.db.*;
import cs.system.data.*;

class AdoNet
{
	public static function create(cnx:IDbConnection, dbName:String):Connection
	{
		return new AdoConnection(cnx,dbName);
	}
}

private class AdoConnection implements Connection
{
	private static var ids = 0;
	private var id:Int;

	private var cnx:IDbConnection;
	//escape handling
	private var escapeRegex:EReg;
	private var escapes:Array<IDbDataParameter>;
	private var name:String;
	private var command:IDbCommand;
	private var transaction:IDbTransaction;

	public function new(cnx,name:String)
	{
		this.id = cs.system.threading.Interlocked.Increment(ids);
		this.cnx = cnx;
		this.name = name;
		this.escapes = [];
		this.command = cnx.CreateCommand();
		this.escapeRegex = ~/@HX_ESCAPE(\d+)_(\d+)/;
	}

	public function close() : Void
	{
		cnx.Close();
	}

	public function escape(s:String):String
	{
		var param = command.CreateParameter();
		var name = "@HX_ESCAPE" + id + "_" +escapes.push(param) + "";
		param.ParameterName = name;
		param.Value = s;
		return name;
	}

	public function quote(s:String):String
	{
		var param = command.CreateParameter();
		var name = "@HX_ESCAPE" + id + "_" +escapes.push(param) + "";
		param.ParameterName = name;
		param.Value = s;
		return name;
	}

	public function addValue(s:StringBuf, v:Dynamic)
	{
		if (Std.is(v, Date))
		{
			v = Std.string(v);
		} else if (Std.is(v, haxe.io.Bytes)) {
			var bt:haxe.io.Bytes = v;
			v = bt.getData();
		}
		var param = command.CreateParameter();
		var name = "@HX_ESCAPE" + id + "_" +escapes.push(param) + "";
		param.ParameterName = name;
		param.Value = v;
		s.add(name);
	}

	public function lastInsertId():Int
	{
		var ret = cnx.CreateCommand();
		ret.CommandText = switch(name) {
			case 'SQLite':
				'SELECT last_insert_rowid()';
			case _:
				'SELECT @@IDENTITY';
		}
		ret.CommandType = CommandType.Text;
		var r = cast ret.ExecuteScalar();
		ret.Dispose();

		return r;
	}

	public function dbName() : String
	{
		return name;
	}

	public function startTransaction() : Void
	{
		if (this.transaction != null)
			throw 'Transaction already active';
		this.transaction = cnx.BeginTransaction();
	}

	public function commit() : Void
	{
		if (this.transaction == null)
			throw 'No transaction was initiated';
		this.transaction.Commit();
	}

	public function rollback() : Void
	{
		if (this.transaction == null)
			throw 'No transaction was initiated';
		this.transaction.Rollback();
	}

	private static function getFirstStatement(s:String)
	{
		var buf = new StringBuf();
		var hasData = false;
		var chr = 0,
				i = 0;
		inline function getch() return chr = StringTools.fastCodeAt(s,i++);
		while ( !StringTools.isEof(getch()) )
		{
			inline function peek() { var c = StringTools.fastCodeAt(s,i); if (StringTools.isEof(c)) break; return c; }
			switch(chr)
			{
				case ' '.code | '\t'.code | '\n'.code:
					if (hasData)
						return buf.toString();
				case '-'.code if (peek() == '-'.code):
					if (hasData)
						return buf.toString();
					while (!StringTools.isEof(getch()))
					{
						if (chr == '\n'.code) break;
					}
				case '#'.code:
					if (hasData)
						return buf.toString();
					while (!StringTools.isEof(getch()))
					{
						if (chr == '\n'.code) break;
					}
				case '/'.code if (peek() == '*'.code):
					i++;
					if (hasData)
						return buf.toString();
					while (!StringTools.isEof(getch()))
					{
						if (chr == '*'.code && peek() == '/'.code)
						{
							i++;
							break;
						}
					}
				case _:
					hasData = true;
					buf.addChar(chr);
			}
		}
		return buf.toString();
	}

	public function request( s : String ) : ResultSet
	{
		var newst = new StringBuf();
		//cycle through the request string, adding any @HX_ESCAPE reference to the command
		var ret:ResultSet = null;
		var r = escapeRegex;
		var myid = id + "", escapes = escapes, elen = escapes.length;
		var cmd = this.command;
		try
		{
			while (r.match(s))
			{
				var id = r.matched(1);
#if debug
				if (id != myid) throw "Request quotes are only valid for one single request; They can't be cached.";
#end

				newst.add(r.matchedLeft());
				var eid = Std.parseInt(r.matched(2));
#if debug
				if (eid == null || eid > elen)
					throw "Invalid request quote ID " + eid;
#end
				cmd.Parameters.Add(escapes[eid - 1]);
				newst.add(escapes[eid-1].ParameterName);
				s = r.matchedRight();
			}
			newst.add(s);

			s = newst.toString();
			cmd.CommandText = s;

			var stmt = getFirstStatement(s).toLowerCase();
			if (stmt == 'select')
			{
				ret = new AdoResultSet( cmd.ExecuteReader() );
			} else {
				cmd.ExecuteNonQuery();
				ret = EmptyResultSet.empty;
			}

			if (escapes.length != 0)
				this.escapes = [];
			this.id = cs.system.threading.Interlocked.Increment(ids);
			cmd.Dispose();
			this.command = cnx.CreateCommand();
			return ret;
		}
		catch(e:Dynamic)
		{
			if (escapes.length != 0)
				this.escapes = [];
			this.id = cs.system.threading.Interlocked.Increment(ids);
			try { cmd.Dispose(); } catch(e:Dynamic) {}
			this.command = cnx.CreateCommand();
			cs.Lib.rethrow(e);
		}
		return null;
	}
}

private class AdoResultSet implements ResultSet
{
	public var length(get,null) : Int;
	public var nfields(get,null) : Int;

	private var reader:IDataReader;
	private var didNext:Bool;
	private var names:Array<String>;
	private var types:Array<Class<Dynamic>>;

	public function new(reader)
	{
		this.reader = reader;
		this.names = [ for (i in 0...reader.FieldCount) reader.GetName(i) ];
		this.types = [ for (i in 0...names.length) cs.Lib.fromNativeType(reader.GetFieldType(i)) ];
	}

	private function get_length()
	{
		return reader.Depth;
	}

	private function get_nfields()
	{
		return names.length;
	}

	public function hasNext() : Bool
	{
		didNext = true;
		return reader.Read();
	}

	public function next() : Dynamic
	{
		if (!didNext && !hasNext())
			return null;
		didNext = false;
		var ret = {}, names = names, types = types;
		for (i in 0...names.length)
		{
			var name = names[i], t = types[i], val:Dynamic = null;
			if (reader.IsDBNull(i)) {
				val = null;
			} else if (t == cs.system.Single) {
				val = reader.GetDouble(i);
			} else if (t == cs.system.DateTime || t == cs.system.TimeSpan) {
				var d = reader.GetDateTime(i);
				if (d != null)
					val = @:privateAccess Date.fromNative(d);
			} else if (t == cs.system.DBNull) {
				val = null;
			} else if (t == cs.system.Byte) {
				var v2:cs.StdTypes.UInt8 = reader.GetValue(i);
				val = cast(v2,Int);
			} else if (Std.string(t) == 'System.Byte[]') {
				val = haxe.io.Bytes.ofData(reader.GetValue(i));
			} else {
				val = reader.GetValue(i);
			}
			if (Std.is(val,cs.system.DBNull))
				val = null;
			Reflect.setField(ret, name, val);
		}
		return ret;
	}

	public function results() : List<Dynamic>
	{
		var l = new List();
		while (hasNext())
			l.add(next());
		return l;
	}

	public function getResult( n : Int ) : String
	{
		return reader.GetString(n);
	}

	public function getIntResult( n : Int ) : Int
	{
		return reader.GetInt32(n);
	}

	public function getFloatResult( n : Int ) : Float
	{
		return reader.GetDouble(n);
	}

	public function getFieldsNames() : Null<Array<String>>
	{
		return names;
	}

}

private class EmptyResultSet implements ResultSet
{
	public static var empty = new EmptyResultSet();
	public function new()
	{
	}

	public var length(get,null) : Int;
	public var nfields(get,null) : Int;

	private function get_length()
	{
		return 0;
	}

	private function get_nfields()
	{
		return 0;
	}

	public function hasNext() : Bool return false;
	public function next() : Dynamic return null;
	public function results() : List<Dynamic> return new List();
	public function getResult( n : Int ) : String return null;
	public function getIntResult( n : Int ) : Int return 0;
	public function getFloatResult( n : Int ) : Float return 0;
	public function getFieldsNames() : Null<Array<String>> return null;
}
