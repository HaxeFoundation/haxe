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

class TableCreate {

	static function autoInc( dbName ) {
		// on SQLite, autoincrement is necessary to be primary key as well
		return dbName == "SQLite" ? "PRIMARY KEY AUTOINCREMENT" : "AUTO_INCREMENT";
	}

	public static function getTypeSQL( t : RecordType, dbName : String ) {
		return switch( t ) {
		case DId: "INTEGER "+autoInc(dbName);
		case DUId: "INTEGER UNSIGNED "+autoInc(dbName);
		case DInt, DEncoded: "INTEGER";
		case DUInt: "INTEGER UNSIGNED";
		case DTinyInt: "TINYINT";
		case DTinyUInt, DEnum(_): "TINYINT UNSIGNED";
		case DSmallInt: "SMALLINT";
		case DSmallUInt: "SMALLINT UNSIGNED";
		case DMediumInt: "MEDIUMINT";
		case DMediumUInt: "MEDIUMINT UNSIGNED";
		case DSingle: "FLOAT";
		case DFloat: "DOUBLE";
		case DBool: "TINYINT(1)";
		case DString(n): "VARCHAR("+n+")";
		case DDate: "DATE";
		case DDateTime: "DATETIME";
		case DTimeStamp: "TIMESTAMP DEFAULT 0";
		case DTinyText: "TINYTEXT";
		case DSmallText: "TEXT";
		case DText, DSerialized: "MEDIUMTEXT";
		case DSmallBinary: "BLOB";
		case DBinary, DNekoSerialized, DData: "MEDIUMBLOB";
		case DLongBinary: "LONGBLOB";
		case DBigInt: "BIGINT";
		case DBigId: "BIGINT "+autoInc(dbName);
		case DBytes(n): "BINARY(" + n + ")";
		case DFlags(fl, auto): getTypeSQL(auto ? (fl.length <= 8 ? DTinyUInt : (fl.length <= 16 ? DSmallUInt : (fl.length <= 24 ? DMediumUInt : DInt))) : DInt, dbName);
		case DNull, DInterval: throw "assert";
		};
	}

	public static function create( manager : sys.db.Manager<Dynamic>, ?engine ) {
		function quote(v:String):String {
			return untyped manager.quoteField(v);
		}
		var cnx : Connection = untyped manager.getCnx();
		if( cnx == null )
			throw "SQL Connection not initialized on Manager";
		var dbName = cnx.dbName();
		var infos = manager.dbInfos();
		var sql = "CREATE TABLE " + quote(infos.name) + " (";
		var decls = [];
		var hasID = false;
		for( f in infos.fields ) {
			switch( f.t ) {
			case DId:
				hasID = true;
			case DUId, DBigId:
				hasID = true;
				if( dbName == "SQLite" )
					throw "S" + Std.string(f.t).substr(1)+" is not supported by " + dbName + " : use SId instead";
			default:
			}
			decls.push(quote(f.name)+" "+getTypeSQL(f.t,dbName)+(f.isNull ? "" : " NOT NULL"));
		}
		if( dbName != "SQLite" || !hasID )
			decls.push("PRIMARY KEY ("+Lambda.map(infos.key,quote).join(",")+")");
		sql += decls.join(",");
		sql += ")";
		if( engine != null )
			sql += "ENGINE="+engine;
		cnx.request(sql);
	}

	public static function exists( manager : sys.db.Manager<Dynamic> ) : Bool {
		var cnx : Connection = untyped manager.getCnx();
		if( cnx == null )
			throw "SQL Connection not initialized on Manager";
		try {
			cnx.request("SELECT * FROM `"+manager.dbInfos().name+"` LIMIT 1");
			return true;
		} catch( e : Dynamic ) {
			return false;
		}
	}
}