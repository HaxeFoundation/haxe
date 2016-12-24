package php.db;

import php.*;
import haxe.extern.EitherType;

@:native('SQLite3Stmt')
extern class SQLite3Stmt {
	function bindParam(sql_param:EitherType<String,Int>, param:Ref<Dynamic>, ?type:Int) : Bool;
	function bindValue(sql_param:EitherType<String,Int>, value:Dynamic, ?type:Int) : Bool;
	function clear() : Bool;
	function close() : Bool;
	function execute() : SQLite3Result;
	function paramCount() : Int;
	function readOnly() : Bool;
	function reset() : Bool;
}