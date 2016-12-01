package php.db;

import php.*;
import haxe.extern.*;

/**
    @see http://php.net/manual/en/class.mysqli-result.php
**/
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
    function fetch_field_direct( fieldnr:Int ) : MysqliFieldInfo;
    function fetch_field() : MysqliFieldInfo;
    function fetch_fields() : NativeIndexedArray<MysqliFieldInfo>;
    function fetch_object( ?class_name:String = "stdClass", ?params:NativeArray ) : {};
    function fetch_row() : NativeIndexedArray<String>;
    function field_seek( fieldnr:Int ) : Bool;
    function free() : Void;
}

typedef MysqliFieldInfo = {
    name : String,
    orgname : String,
    table : String,
    orgtable : String,
    max_length : Int,
    length : Int,
    charsetnr : Int,
    flags : Int,
    type : Int,
    decimals : Int
}