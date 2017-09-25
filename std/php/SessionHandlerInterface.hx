package php;

/**
	@see http://php.net/manual/en/class.sessionhandlerinterface.php
**/
@:native('SessionHandlerInterface')
extern interface SessionHandlerInterface {
	function close() : Bool;
	function destroy( session_id:String ) : Bool;
	function gc( maxlifetime:Int ) : Bool;
	function open( save_path:String, session_name:String ) : Bool;
	function read( session_id:String ) : String;
	function write( session_id:String, session_data:String ) : Bool;
}