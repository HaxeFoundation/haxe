package php7.db;

import php7.*;
import haxe.extern.*;

@:native('Myslqi_result')
extern class Mysqli_result implements Traversable {
    var current_field  (default,null) : Int;
    var field_count (default,null) : Int;
    var lengths (default,null) : EitherType<Bool, NativeIndexedArray<Int>>;
    var num_rows (default,null) : Int;

    function data_seek( offset:Int ) : Bool;
    function fetch_all( ?resulttype:Int ) : NativeArray;
    function fetch_array( ?resulttype:Int ) : NativeArray;
    function fetch_assoc() : NativeAssocArray<String>;
    function fetch_field_direct( fieldnr:Int ) : {};
    function fetch_field() : {};
    function fetch_fields() : NativeIndexedArray<{}>;
    function fetch_object( ?class_name:String = "stdClass", ?params:NativeArray ) : {};
    function fetch_row() : NativeIndexedArray<String>;
    function field_seek( fieldnr:Int ) : Bool;
    function free() : Void;
}