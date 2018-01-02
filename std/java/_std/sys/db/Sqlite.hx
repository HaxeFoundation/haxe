/*
 * Copyright (C)2005-2018 Haxe Foundation
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

class Sqlite
{
	static var init = false;
	/**
		Opens a new SQLite connection on the specified path.
		Note that you will need a SQLite JDBC driver (like https://bitbucket.org/xerial/sqlite-jdbc).
	**/
	public static function open(file:String):sys.db.Connection
	{
		if (!init)
		{
			try java.lang.Class.forName("org.sqlite.JDBC") catch(e:Dynamic) throw e;
			init = true;
		}
		try
		{
			var cnx = java.sql.DriverManager.getConnection("jdbc:sqlite:" + file);
			return java.db.Jdbc.create(cnx);
		} catch(e:Dynamic) throw e;
	}
}
