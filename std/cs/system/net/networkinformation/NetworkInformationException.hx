package cs.system.net.networkinformation;

/** The exception that is thrown when an error occurs while retrieving network information. */
@:native("System.Net.NetworkInformation.NetworkInformationException")
extern class NetworkInformationException extends cs.system.componentmodel.Win32Exception {
	@:overload(function():Void {})
	function new(errorCode:Int):Void;
}
