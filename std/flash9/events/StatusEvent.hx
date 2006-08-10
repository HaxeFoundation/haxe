package flash.events;

extern class StatusEvent extends flash.events.Event {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?code : String, ?level : String) : Void;
	var code : String;
	var level : String;
	private var m_code : String;
	private var m_level : String;
	static var STATUS : String;
}
