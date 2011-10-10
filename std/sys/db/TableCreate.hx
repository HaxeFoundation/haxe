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

	public static function getTypeSQL( t : SpodType ) {
		return switch( t ) {
		case DId: "INT AUTO_INCREMENT";
		case DUId: "INT UNSIGNED AUTO_INCREMENT";
		case DInt, DEncoded, DFlags(_): "INT";
		case DTinyInt: "TINYINT";
		case DUInt: "INT UNSIGNED";
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
		case DBinary, DNekoSerialized: "MEDIUMBLOB";
		case DLongBinary: "LONGBLOB";
		case DBigInt: "BIGINT";
		case DBigId: "BIGINT AUTO_INCREMENT";
		case DBytes(n): "BINARY(" + n + ")";
		case DNull, DInterval: throw "assert";
		};
	}

	public static function create( manager : sys.db.Manager<Dynamic>, ?engine ) {
		function quote(v:String):String {
			return untyped manager.quoteField(v);
		}
		var infos = manager.dbInfos();
		var sql = "CREATE TABLE "+quote(infos.name)+ " (";
		var decls = [];
		for( f in infos.fields )
			decls.push(quote(f.name)+" "+getTypeSQL(f.t)+(f.isNull ? "" : " NOT NULL"));
		decls.push("PRIMARY KEY ("+Lambda.map(infos.key,quote).join(",")+")");
		sql += decls.join(",");
		sql += ")";
		if( engine != null )
			sql += "ENGINE="+engine;
		var cnx : Connection = untyped manager.getCnx();
		if( cnx == null )
			throw "SQL Connection not initialized on Manager";
		cnx.request(sql);
	}

}