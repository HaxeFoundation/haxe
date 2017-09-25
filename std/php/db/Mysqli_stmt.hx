package php.db;

import haxe.extern.*;
import php.*;

@:native('Mysqli_stmt')
extern class Mysqli_stmt {
    var affected_rows (default,null) : Int;
    var errno (default,null) : Int;
    var error_list (default,null) : NativeArray;
    var error (default,null) : String;
    var field_count (default,null) : Int;
    var insert_id (default,null) : Int;
    var num_rows (default,null) : Int;
    var param_count (default,null) : Int;
    var sqlstate (default,null) : String;

    function new( link:Mysqli, ?query:String ) : Void;
    function attr_get( attr:Int ) : Int;
    function attr_set( attr:Int , mode:Int ) : Bool;
    function bind_param( types:String , var1:Ref<Dynamic>, args:Rest<Ref<Dynamic>> ) : Bool;
    function bind_result( var1:Ref<Dynamic>, args:Rest<Ref<Dynamic>> ) : Bool;
    function close() : Bool;
    function data_seek( offset:Int ) : Void;
    function execute() : Bool;
    function fetch() : Bool;
    function free_result() : Void;
    function get_result() : Mysqli_result;
    function get_warnings( stmt:Mysqli_stmt ) : {};
    function prepare( query:String ) : Bool;
    function reset() : Bool;
    function result_metadata() : Mysqli_result;
    function send_long_data( param_nr:Int , data:String ) : Bool;
    function store_result() : Bool;
}