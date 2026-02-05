package cs.system.xml.serialization;

/** Provides data for the known, but unreferenced, object found in an encoded SOAP XML stream during deserialization. */
@:native("System.Xml.Serialization.UnreferencedObjectEventArgs")
extern class UnreferencedObjectEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the ID of the object.
	 * @return The ID of the object.
	 */
	var UnreferencedId(default, never):String;
	/**
	 * Gets the deserialized, but unreferenced, object.
	 * @return The deserialized, but unreferenced, object.
	 */
	var UnreferencedObject(default, never):Dynamic;
	function new(o:Dynamic, id:String):Void;
}
