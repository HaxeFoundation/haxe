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
import haxe.crypto.BaseCode;

private typedef SqliteConnectionHandle = hl.Abstract<"sqlite_database">;
private typedef SqliteResultHandle = hl.Abstract<"sqlite_result">;

@:hlNative("sqlite")
private class SqliteLib
{
	public static function connect( path : hl.Bytes ) : SqliteConnectionHandle { return null; }
	public static function close( c : SqliteConnectionHandle ) : Void { }
	public static function request( c : SqliteConnectionHandle, sql : hl.Bytes ) : SqliteResultHandle { return null; }
	public static function last_id( c : SqliteConnectionHandle ) : Int { return 0; }
	
	public static function result_next( c : SqliteResultHandle ) : hl.NativeArray<Dynamic> { return null; }
	
	public static function result_get( c : SqliteResultHandle, n : Int ) : Null<hl.Bytes> { return null; }
	public static function result_get_int( c : SqliteResultHandle, n : Int ) : Null<Int> { return 0; }
	public static function result_get_float( c : SqliteResultHandle, n : Int ) : Null<Float> { return .0; }
	public static function result_get_length( c : SqliteResultHandle ) : Null<Int> { return 0; }
	public static function result_get_nfields( c : SqliteResultHandle ) : Int { return 0; }
	public static function result_get_fields( c : SqliteResultHandle ) : hl.NativeArray<hl.Bytes> { return null; }
}


@:access(Sys)
@:access(String)
private class SqliteConnection implements Connection
{
	
	var c : SqliteConnectionHandle;

	public function new( file : String )
	{
		c = SqliteLib.connect(Sys.getPath(file));
	}
	
	public function close( ) : Void
	{
		SqliteLib.close(c);
	}
	
	public function request( s : String ) : ResultSet
	{
		try
		{
			var r : SqliteResultHandle = SqliteLib.request(c, s.bytes);
			
			return new SqliteResultSet(r);
		}
		catch ( e : String )
		{
			throw 'Error while executing $s ($e)';
		}
		
		return null;
	}
	
	public function escape( s : String ) : String
	{
		return s.split("'").join("''");
	}
	
	public function quote( s : String ) : String
	{
		if( s.indexOf("\000") >= 0 )
			return "x'" + BaseCode.encode(s, "0123456789ABCDEF") + "'";
		
		return "'" + s.split("'").join("''") + "'";
	}
	
	public function addValue( s : StringBuf, v : Dynamic ) : Void
	{
		switch( Type.typeof(v) )
		{
			case TNull, TInt: s.add(v);
			case TBool: s.add( v ? 1 : 0);
			case _: s.add(quote(Std.string(v)));
		}
	}
	
	public function lastInsertId( ) : Int
	{
		return SqliteLib.last_id(c);
	}
	
	public function dbName( ) : String
	{
		return "SQLite";
	}
	
	public function startTransaction( ) : Void
	{
		request("BEGIN TRANSACTION");
	}
	
	public function commit( ) : Void
	{
		request("COMMIT");
		startTransaction(); // match mysql usage
	}
	
	public function rollback( ) : Void
	{
		request("ROLLBACK");
		startTransaction(); // match mysql usage
	}
}



@:access(String)
private class SqliteResultSet implements ResultSet
{
	public var length(get,null) : Int;
	public var nfields(get,null) : Int;
	
	var names : Array<String>;
	var cache : List<Dynamic>;
	
	var r : SqliteResultHandle;
	
	public function new( r : SqliteResultHandle )
	{
		cache = new List();
		this.r = r;
		hasNext(); // execute the request
	}
	
	function get_length( ) : Int
	{
		if ( nfields != 0 )
		{
			while ( true )
			{
				var c = doNext();
				if( c == null )
					break;
				
				cache.add(c);
			}
			
			return cache.length;
		}
		
		return SqliteLib.result_get_length(r);
	}
	
	function get_nfields( ) : Int
	{
		return SqliteLib.result_get_nfields(r);
	}
	
	public function hasNext( ) : Bool
	{
		var c = next();
		if( c == null )
			return false;
		
		cache.push(c);
		
		return true;
	}
	
	public function next( ) : Dynamic
	{
		var c = cache.pop();
		if( c != null )
			return c;
		
		return doNext();
	}
	
	private function doNext( ) : Dynamic
	{
		var o : Dynamic = {};
		var a = SqliteLib.result_next(r);
		if( a == null )
			return null;
		
		var names = getFieldsNames();
		var i = 0 ;
		var l = names.length;
		while ( i < l )
		{
			var n : String = names[i];
			var v : Dynamic = a[i];
			if ( hl.Type.getDynamic(v).kind == hl.Type.TypeKind.HBytes )
				Reflect.setField(o, n, String.fromUCS2(v));
			else
				Reflect.setField(o, n, v);
			i++;
		}
		return o;
	}
	
	public function results( ) : List<Dynamic>
	{
		var l = new List();
		while ( true )
		{
			var c = next();
			if( c == null )
				break;
			
			l.add(c);
		}
		
		return l;
	}
	
	public function getResult( n : Int ) : String
	{
		var bytes = SqliteLib.result_get(r, n);
		if ( bytes == null )
			return null;
		
		return String.fromUCS2(bytes);
	}
	
	public function getIntResult( n : Int ) : Int
	{
		return SqliteLib.result_get_int(r,n);
	}
	
	public function getFloatResult( n : Int ) : Float
	{
		return SqliteLib.result_get_float(r,n);
	}
	
	public function getFieldsNames( ) : Array<String>
	{
		if ( this.names != null )
			return this.names;
		
		this.names = [];
		var names = SqliteLib.result_get_fields(r);
		var i = 0;
		var l = names.length;
		while ( i < l )
		{
			var name = String.fromUCS2(names[i]);
			this.names.push(name);
			i++;
		}
		
		return this.names;
	}
}

@:coreApi class Sqlite
{
	public static function open( file : String ) : Connection
	{
		return new SqliteConnection(file);
	}
}