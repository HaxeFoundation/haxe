package php.db;

import php.*;
import haxe.extern.EitherType;

@:native('SQLite3Result')
extern class SQLite3Result {
	function columnName(column_number:Int) : String;
	function columnType(column_number:Int) : Int;
	function fetchArray(?mode:Int) : EitherType<Bool,NativeAssocArray<String>>;
	function finalize() : Bool;
	function numColumns() : Int;
	function reset() : Bool;
}