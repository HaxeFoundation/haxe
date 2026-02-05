package cs.system.componentmodel;

/** Specifies which event is raised on initialization. This class cannot be inherited. */
@:native("System.ComponentModel.InitializationEventAttribute")
extern class InitializationEventAttribute extends cs.system.Attribute {
	/**
	 * Gets the name of the initialization event.
	 * @return The name of the initialization event.
	 */
	var EventName(default, never):String;
	function new(eventName:String):Void;
}
