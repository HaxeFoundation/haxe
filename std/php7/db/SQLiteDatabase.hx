package php7.db;

import php7.*;

/**
    @see http://php.net/manual/en/ref.sqlite.php
**/
@:native('SQLiteDatabase')
extern class SQLiteDatabase {
    @:final function new( filename:String, mode:Int = 0666, ?error_message:Ref<String> ) : Void;
    function query( query:String, ?result_type:Int, error_msg:Ref<String> ) : SQLiteResult;
    function queryExec( query:String, ?error_msg:Ref<String> ) : Bool;
    function arrayQuery( query:String, ?result_type:Int, decode_binary:Bool = true) : NativeArray;
    function singleQuery( query:String, ?first_row_only:Bool, ?decode_binary:Bool ) : NativeArray;
}