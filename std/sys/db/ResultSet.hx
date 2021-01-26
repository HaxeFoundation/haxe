/*
 * Copyright (C)2005-2019 Haxe Foundation
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

interface ResultSet {
	/**
		Get amount of rows left in this set.
		Depending on a database management system accessing this field may cause
		all rows to be fetched internally. However, it does not affect `next` calls.
	**/
	var length(get, null):Int;
	/**
		Amount of columns in a row.
		Depending on a database management system may return `0` if the query
		did not match any rows.
	**/
	var nfields(get, null):Int;

	/**
		Tells whether there is a row to be fetched.
	**/
	function hasNext():Bool;
	/**
		Fetch next row.
	**/
	function next():Dynamic;
	/**
		Fetch all the rows not fetched yet.
	**/
	function results():List<Dynamic>;
	/**
		Get the value of `n`-th column of the current row.
		Throws an exception if the re
	**/
	function getResult(n:Int):String;
	/**
		Get the value of `n`-th column of the current row as an integer value.
	**/
	function getIntResult(n:Int):Int;
	/**
		Get the value of `n`-th column of the current row as a float value.
	**/
	function getFloatResult(n:Int):Float;
	/**
		Get the list of column names.
		Depending on a database management system may return `null` if there's no
		more rows to fetch.
	**/
	function getFieldsNames():Null<Array<String>>;
}
