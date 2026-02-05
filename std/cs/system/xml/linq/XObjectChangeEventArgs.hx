package cs.system.xml.linq;

/** Provides data for the  and  events. */
@:native("System.Xml.Linq.XObjectChangeEventArgs")
extern class XObjectChangeEventArgs extends cs.system.EventArgs {
	/** Event argument for an  change event. */
	static var Add(default, never):cs.system.xml.linq.XObjectChangeEventArgs;
	/** Event argument for a  change event. */
	static var Name(default, never):cs.system.xml.linq.XObjectChangeEventArgs;
	/** Event argument for a  change event. */
	static var Remove(default, never):cs.system.xml.linq.XObjectChangeEventArgs;
	/** Event argument for a  change event. */
	static var Value(default, never):cs.system.xml.linq.XObjectChangeEventArgs;
	/**
	 * Gets the type of change.
	 * @return An  that contains the type of change.
	 */
	var ObjectChange(default, never):cs.system.xml.linq.XObjectChange;
	function new(objectChange:cs.system.xml.linq.XObjectChange):Void;
}
