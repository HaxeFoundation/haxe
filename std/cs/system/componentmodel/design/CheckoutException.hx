package cs.system.componentmodel.design;

/** The exception that is thrown when an attempt to check out a file that is checked into a source code management program is canceled or fails. */
@:native("System.ComponentModel.Design.CheckoutException")
extern class CheckoutException extends cs.system.runtime.interopservices.ExternalException {
	/** Initializes a new instance of the  class that specifies that the check out was canceled. This field is read-only. */
	static var Canceled(default, never):cs.system.componentmodel.design.CheckoutException;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	function new(message:String, errorCode:Int):Void;
}
