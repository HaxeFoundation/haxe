package php.db;

import php.NativeArray;

@:native('Mysqli_driver')
extern class Mysqli_driver {
    var client_info (default,null) : String ;
    var client_version (default,null) : String ;
    var driver_version (default,null) : String ;
    var embedded (default,null) : String ;
    var reconnect (default,null) : Bool ;
    var report_mode (default,null) : Int ;

    function embedded_server_end() : Void;
    function embedded_server_start( start:Bool, arguments:NativeArray, groups:NativeArray ) : Bool;
}