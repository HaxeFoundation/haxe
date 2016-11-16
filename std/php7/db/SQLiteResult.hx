package php7.db;

import haxe.extern.EitherType;
import php7.*;

@:native('SQLiteResult')
extern class SQLiteResult {
    function fetch( ?result_type:Int, decode_binary:Bool = true ) : EitherType<Bool,NativeArray>;
    function fetchObject( ?class_name:String, ?ctor_params:NativeArray, decode_binary:Bool = true ) : EitherType<Bool,{}>;
    function fetchSingle( decode_binary:Bool = true ) : String;
    function fetchAll( ?result_type:Int, decode_binary:Bool = true ) : NativeArray;
    function column( ?index_or_name:EitherType<String,Int>, decode_binary:Bool = true ) : Dynamic;
    function numFields() : Int;
    function fieldName( field_index:Int ) : String;
    function current( ?result_type:Int, decode_binary:Bool = true ) : EitherType<Bool,NativeArray>;
    function key() : Int;
    function next() : Bool;
    function valid() : Bool;
    function rewind() : Bool;
    function prev() : Bool;
    function hasPrev() : Bool;
    function numRows() : Bool;
    function numRows() : Int;
}