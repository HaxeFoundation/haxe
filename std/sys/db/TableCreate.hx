/*
 * Copyright (c) 2005-2011, The haXe Project Contributors
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
import sys.db.SpodInfos;

class TableCreate {

	static function autoInc( dbName ) {
		// on SQLite, autoincrement is necessary to be primary key as well
		return dbName == "SQLite" ? "PRIMARY KEY AUTOINCREMENT" : "AUTO_INCREMENT";
	}

	public static function getTypeSQL( t : SpodType, dbName : String ) {
		return switch( t ) {
		case DId: "INTEGER "+autoInc(dbName);
		case DUId: "INTEGER UNSIGNED "+autoInc(dbName);
		case DInt, DEncoded: "INTEGER";
		case DUInt: "INTEGER UNSIGNED";
		case DTinyInt: "TINYINT";
		case DTinyUInt: "TINYINT UNSIGNED";
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