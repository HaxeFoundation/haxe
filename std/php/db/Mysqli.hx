package php.db;

import haxe.extern.*;
import php.*;
import haxe.Constraints.Function;

/**
    @see http://php.net/manual/en/class.mysqli.php
**/
@:native('Mysqli')
extern class Mysqli {
    var affected_rows (default,null) : Int;
    var client_info (default,null) : String;
    var client_version (default,null) : Int;
    var connect_errno (default,null) : Int;
    var connect_error (default,null) : String;
    var errno (default,null) : Int;
    var error_list (default,null) : NativeAssocArray<Scalar>;
    var error (default,null) : String;
    var field_count (default,null) : Int;
    var host_info (default,null) : String;
    var protocol_version (default,null) : String;
    var server_info (default,null) : String;
    var server_version (default,null) : Int;
    var info (default,null) : String;
    var insert_id (default,null) : EitherType<Int,String>;
    var sqlstate (default,null) : String;
    var thread_id (default,null) : Int;
    var warning_count (default,null) : Int;

    static function poll( read:Ref<NativeArray> , error:Ref<NativeArray> , reject:Ref<NativeArray> , sec:Int, ?usec:Int ) : Int;

    function new( ?host:String, ?username:String, ?passwd:String, dbname:String = "", ?port:Int, ?socket:String ) : Void;
    function autocommit( mode:Bool ) : Bool;
    function begin_transaction( ?flags:Int, ?name:String ) : Bool;
    function change_user( user:String, password:String, database:String ) : Bool;
    function character_set_name() : String;
    function close() : Bool;
    function commit( ?flags:Int, ?name:String ) : Bool;
    function debug( message:String ) : Bool;
    function dump_debug_info() : Bool;
    function get_charset() : {charset:String, collation:String, dir:String, min_length:Int, number:Int, state:Int};
    function get_client_info() : String;
    function get_connection_stats() : Bool;
    function get_warnings() : Mysqli_warning;
    function init() : Mysqli;
    function kill( processid:Int ) : Bool;
    function more_results() : Bool;
    function multi_query( query:String ) : Bool;
    function next_result() : Bool;
    function options( option:Int , value:Scalar ) : Bool;
    function ping() : Bool;
    function prepare( query:String ) : Mysqli_stmt;
    function query( query:String, ?resultmode:Int ) : EitherType<Bool,Mysqli_result>;
    function real_connect( ?host:String, ?username:String, ?passwd:String, ?dbname:String, ?port:Int, ?socket:String, ?flags:Int ) : Bool;
    function escape_string( escapestr:String ) : String;
    function real_query( query:String ) : Bool;
    function reap_async_query() : Mysqli_result;
    function refresh( options:Int ) : Bool;
    function rollback( ?flags:Int, ?name:String ) : Bool;
    function rpl_query_type( query:String ) : Int;
    function select_db( dbname:String ) : Bool;
    function send_query( query:String ) : Bool;
    function set_charset( charset:String ) : Bool;
    function set_local_infile_handler( read_func:Function ) : Bool;
    function ssl_set( key:String, cert:String, ca:String, capath:String, cipher:String) : Bool;
    function stat() : String;
    function stmt_init() : Mysqli_stmt;
    function store_result( ?option:Int ) : Mysqli_result;
    function use_result() : Mysqli_result;
}