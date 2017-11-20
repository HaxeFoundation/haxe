package php.db;

import haxe.Constraints;
import php.*;
import haxe.extern.EitherType;

@:native('SQLite3')
extern class SQLite3 {
	static function version() : NativeArray;
	static function escapeString(value:String) : String;
	function busyTimeout(msecs:Int) : Bool;
	function changes() : Int;
	function close() : Bool;
	function new(filename:String, ?flags:Int, encryption_key:String = null) : Void;
	function createAggregate(name:String, step_callback:Function, final_callback:Function, argument_count:Int = -1) : Bool;
	function createCollation(name:String, callback:Function) : Bool;
	function createFunction(name:String, callback:Function, argument_count:Int = -1) : Bool;
	function enableExceptions( enableExceptions:Bool = false) : Bool;
	function exec(query:String) : Bool;
	function lastErrorCode() : Int;
	function lastErrorMsg() : String;
	function lastInsertRowID() : Int;
	function loadExtension(shared_library:String) : Bool;
	function open(filename:String, ?flags:Int, encryption_key:String = null) : Void;
	function prepare(query:String) : SQLite3Stmt;
	function query(query:String) : EitherType<Bool,SQLite3Result>;
	function querySingle(query:String, entire_row:Bool = false) : Dynamic;
}