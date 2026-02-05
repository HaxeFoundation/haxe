package cs.system.xml.schema;

/**
 * Represents the callback method that will handle XML schema validation events and
 * the .
 * @param sender The source of the event. Note Determine the type of a sender
 * before using it in your code. You cannot assume that the sender is an instance
 * of a particular type. The sender is also not guaranteed to not  be null. Always
 * surround your casts with failure handling logic.
 * @param e The event data.
 */
@:native("System.Xml.Schema.ValidationEventHandler")
extern class ValidationEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.xml.schema.ValidationEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.xml.schema.ValidationEventArgs):Void;
}
