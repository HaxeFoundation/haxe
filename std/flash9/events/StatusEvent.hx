package flash.events;

extern class StatusEvent extends Event {
	var code : String;
	var level : String;
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?code : String, ?level : String) : Void;
	static var STATUS : String;
}
