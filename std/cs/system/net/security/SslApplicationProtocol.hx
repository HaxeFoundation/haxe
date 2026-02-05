package cs.system.net.security;

@:native("System.Net.Security.SslApplicationProtocol")
extern class SslApplicationProtocol extends cs.system.ValueType {
	static var Http11(default, never):cs.system.net.security.SslApplicationProtocol;
	static var Http2(default, never):cs.system.net.security.SslApplicationProtocol;
	var Protocol(default, never):cs.system.ReadOnlyMemory<cs.UInt8>;
	@:overload(function(protocol:cs.NativeArray<cs.UInt8>):Void {})
	function new(protocol:String):Void;
	/**
	 * @param left 
	 * @param right 
	 */
	static function op_Equality(left:cs.system.net.security.SslApplicationProtocol, right:cs.system.net.security.SslApplicationProtocol):Bool;
	/**
	 * @param left 
	 * @param right 
	 */
	static function op_Inequality(left:cs.system.net.security.SslApplicationProtocol, right:cs.system.net.security.SslApplicationProtocol):Bool;
	@:overload(function(other:cs.system.net.security.SslApplicationProtocol):Bool {})
	/** @param other  */
	function Equals(obj:Dynamic):Bool;
	function GetHashCode():Int;
	function ToString():String;
}
