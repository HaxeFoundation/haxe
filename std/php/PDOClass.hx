/*
 * Copyright (C)2005-2016 Haxe Foundation
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
package php;

import php.NativeArray;

import php.Lib;

@:native("PDO")
extern class PDOClass
{
	public static var ATTR_CASE(get, never):Int;
	private static inline function get_ATTR_CASE() : Int return untyped __php__("PDO::ATTR_CASE"); 
	
	public static var CASE_LOWER(get, never):Int;
	private static inline function get_CASE_LOWER() : Int return untyped __php__("PDO::CASE_LOWER"); 
	
	public static var CASE_NATURAL(get, never):Int;
	private static inline function get_CASE_NATURAL() : Int return untyped __php__("PDO::CASE_NATURAL"); 
	
	public static var CASE_UPPER(get, never):Int;
	private static inline function get_CASE_UPPER() : Int return untyped __php__("PDO::CASE_UPPER"); 
	
	public static var ATTR_ERRMODE(get, never):Int;
	private static inline function get_ATTR_ERRMODE() : Int return untyped __php__("PDO::ATTR_ERRMODE"); 
	
	public static var ERRMODE_SILENT(get, never):Int;
	private static inline function get_ERRMODE_SILENT() : Int return untyped __php__("PDO::ERRMODE_SILENT"); 
	
	public static var ERRMODE_WARNING(get, never):Int;
	private static inline function get_ERRMODE_WARNING() : Int return untyped __php__("PDO::ERRMODE_WARNING"); 
	
	public static var ERRMODE_EXCEPTION(get, never):Int;
	private static inline function get_ERRMODE_EXCEPTION() : Int return untyped __php__("PDO::ERRMODE_EXCEPTION"); 
	
	public static var ATTR_ORACLE_NULLS(get, never):Int;
	private static inline function get_ATTR_ORACLE_NULLS() : Int return untyped __php__("PDO::ATTR_ORACLE_NULLS"); 
	
	public static var NULL_NATURAL(get, never):Int;
	private static inline function get_NULL_NATURAL() : Int return untyped __php__("PDO::NULL_NATURAL"); 
	
	public static var NULL_EMPTY_STRING(get, never):Int;
	private static inline function get_NULL_EMPTY_STRING() : Int return untyped __php__("PDO::NULL_EMPTY_STRING"); 
	
	public static var NULL_TO_STRING(get, never):Int;
	private static inline function get_NULL_TO_STRING() : Int return untyped __php__("PDO::NULL_TO_STRING"); 
	
	public static var ATTR_STRINGIFY_FETCHES(get, never):Int;
	private static inline function get_ATTR_STRINGIFY_FETCHES() : Int return untyped __php__("PDO::ATTR_STRINGIFY_FETCHES"); 
	
	public static var ATTR_STATEMENT_CLASS(get, never):Int;
	private static inline function get_ATTR_STATEMENT_CLASS() : Int return untyped __php__("PDO::ATTR_STATEMENT_CLASS"); 
	
	public static var ATTR_TIMEOUT(get, never):Int;
	private static inline function get_ATTR_TIMEOUT() : Int return untyped __php__("PDO::ATTR_TIMEOUT"); 
	
	public static var ATTR_AUTOCOMMIT(get, never):Int;
	private static inline function get_ATTR_AUTOCOMMIT() : Int return untyped __php__("PDO::ATTR_AUTOCOMMIT"); 
	
	public static var ATTR_EMULATE_PREPARES(get, never):Int;
	private static inline function get_ATTR_EMULATE_PREPARES() : Int return untyped __php__("PDO::ATTR_EMULATE_PREPARES"); 
	
	public static var ATTR_DEFAULT_FETCH_MODE(get, never):Int;
	private static inline function get_ATTR_DEFAULT_FETCH_MODE() : Int return untyped __php__("PDO::ATTR_DEFAULT_FETCH_MODE"); 
	
	public static var FETCH_ASSOC(get, never):Int;
	private static inline function get_FETCH_ASSOC() : Int return untyped __php__("PDO::FETCH_ASSOC"); 
	
	public static var FETCH_BOTH(get, never):Int;
	private static inline function get_FETCH_BOTH() : Int return untyped __php__("PDO::FETCH_BOTH"); 
	
	public static var FETCH_BOUND(get, never):Int;
	private static inline function get_FETCH_BOUND() : Int return untyped __php__("PDO::FETCH_BOUND"); 
	
	public static var FETCH_INTO(get, never):Int;
	private static inline function get_FETCH_INTO() : Int return untyped __php__("PDO::FETCH_INTO"); 
	
	public static var FETCH_LAZY(get, never):Int;
	private static inline function get_FETCH_LAZY() : Int return untyped __php__("PDO::FETCH_LAZY"); 
	
	public static var FETCH_NAMED(get, never):Int;
	private static inline function get_FETCH_NAMED() : Int return untyped __php__("PDO::FETCH_NAMED"); 
	
	public static var FETCH_NUM(get, never):Int;
	private static inline function get_FETCH_NUM() : Int return untyped __php__("PDO::FETCH_NUM"); 
	
	public static var FETCH_OBJ(get, never):Int;
	private static inline function get_FETCH_OBJ() : Int return untyped __php__("PDO::FETCH_OBJ"); 
	
	public static var FETCH_ORI_NEXT(get, never):Int;
	private static inline function get_FETCH_ORI_NEXT() : Int return untyped __php__("PDO::FETCH_ORI_NEXT"); 
	
	public static var PARAM_STR(get, never):Int;
	private static inline function get_PARAM_STR():Int return untyped __php__("PDO::PARAM_STR");
    
    public static var MYSQL_ATTR_USE_BUFFERED_QUERY(get, never):Int;
	private static inline function get_MYSQL_ATTR_USE_BUFFERED_QUERY():Int return untyped __php__("PDO::MYSQL_ATTR_USE_BUFFERED_QUERY");
    
    public static var MYSQL_ATTR_LOCAL_INFILE(get, never):Int;
	private static inline function get_MYSQL_ATTR_LOCAL_INFILE():Int return untyped __php__("PDO::MYSQL_ATTR_LOCAL_INFILE");
    
    public static var MYSQL_ATTR_INIT_COMMAND(get, never):Int;
	private static inline function get_MYSQL_ATTR_INIT_COMMAND():Int return untyped __php__("PDO::MYSQL_ATTR_INIT_COMMAND");
    
    public static var MYSQL_ATTR_READ_DEFAULT_FILE(get, never):Int;
	private static inline function get_MYSQL_ATTR_READ_DEFAULT_FILE():Int return untyped __php__("PDO::MYSQL_ATTR_READ_DEFAULT_FILE");
    
    public static var MYSQL_ATTR_READ_DEFAULT_GROUP(get, never):Int;
	private static inline function get_MYSQL_ATTR_READ_DEFAULT_GROUP():Int return untyped __php__("PDO::MYSQL_ATTR_READ_DEFAULT_GROUP");
    
    public static var MYSQL_ATTR_MAX_BUFFER_SIZE(get, never):Int;
	private static inline function get_MYSQL_ATTR_MAX_BUFFER_SIZE():Int return untyped __php__("PDO::MYSQL_ATTR_MAX_BUFFER_SIZE");
    
    public static var MYSQL_ATTR_DIRECT_QUERY(get, never):Int;
	private static inline function get_MYSQL_ATTR_DIRECT_QUERY():Int return untyped __php__("PDO::MYSQL_ATTR_DIRECT_QUERY");
    
    public static var MYSQL_ATTR_FOUND_ROWS(get, never):Int;
	private static inline function get_MYSQL_ATTR_FOUND_ROWS():Int return untyped __php__("PDO::MYSQL_ATTR_FOUND_ROWS");
    
    public static var MYSQL_ATTR_IGNORE_SPACE(get, never):Int;
	private static inline function get_MYSQL_ATTR_IGNORE_SPACE():Int return untyped __php__("PDO::MYSQL_ATTR_IGNORE_SPACE");
    
    public static var MYSQL_ATTR_COMPRESS(get, never):Int;
	private static inline function get_MYSQL_ATTR_COMPRESS():Int return untyped __php__("PDO::MYSQL_ATTR_COMPRESS");
    
    public static var MYSQL_ATTR_SSL_CA(get, never):Int;
	private static inline function get_MYSQL_ATTR_SSL_CA():Int return untyped __php__("PDO::MYSQL_ATTR_SSL_CA");
    
    public static var MYSQL_ATTR_SSL_CAPATH(get, never):Int;
	private static inline function get_MYSQL_ATTR_SSL_CAPATH():Int return untyped __php__("PDO::MYSQL_ATTR_SSL_CAPATH");
    
    public static var MYSQL_ATTR_SSL_CERT(get, never):Int;
	private static inline function get_MYSQL_ATTR_SSL_CERT():Int return untyped __php__("PDO::MYSQL_ATTR_SSL_CERT");
    
    public static var MYSQL_ATTR_SSL_CIPHER(get, never):Int;
	private static inline function get_MYSQL_ATTR_SSL_CIPHER():Int return untyped __php__("PDO::MYSQL_ATTR_SSL_CIPHER");
    
    public static var MYSQL_ATTR_SSL_KEY(get, never):Int;
	private static inline function get_MYSQL_ATTR_SSL_KEY():Int return untyped __php__("PDO::MYSQL_ATTR_SSL_KEY");
    
    public static var MYSQL_ATTR_MULTI_STATEMENTS(get, never):Int;
	private static inline function get_MYSQL_ATTR_MULTI_STATEMENTS():Int return untyped __php__("PDO::MYSQL_ATTR_MULTI_STATEMENTS");
    
	@:overload(function(dns : String):Void{})
	@:overload(function(dns : String, username : String):Void{})
	@:overload(function(dns : String, username : String, password : String):Void { } )
	public function new(dns : String, username : String, password : String, driver_options : NativeArray):Void;
	
	public function beginTransaction() : Bool;
	public function commit() : Bool;
	public function errorCode() : Dynamic;
	public function errorInfo() : NativeArray;
	public function exec(statement : String) : Int;
	public function getAttribute(attribute : Int) : Dynamic;
	public function getAvailableDrivers() : NativeArray;
	public function lastInsertId(?name : String) : String;
	
	@:overload(function(statement : String):PDOStatement { })
	public function prepare(statement : String, driver_options : NativeArray) : PDOStatement;
	
	@:overload(function(statement : String, mode : Int):PDOStatement { })
	@:overload(function(statement : String, mode : Int, colno:Int):PDOStatement { })
	@:overload(function(statement : String, mode : Int, object:Dynamic):PDOStatement { })
	@:overload(function(statement : String, mode : Int, classname:String, ctorargs:NativeArray):PDOStatement { })
	public function query(statement : String) : PDOStatement;
	
    @:overload(function(string:String):String {})
	public function quote(string : String, parameter_type : Int) : String;
	public function rollBack() : Bool;
	public function setAttribute(attribute : Int, value : Dynamic) : Bool;
    public function inTransaction():Bool;
}