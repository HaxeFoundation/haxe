package cs.system.xml;

/** Provides data for the , , , ,  and  events. */
@:native("System.Xml.XmlNodeChangedEventArgs")
extern class XmlNodeChangedEventArgs extends cs.system.EventArgs {
	/**
	 * Gets a value indicating what type of node change event is occurring.
	 * @return One of the enumeration values that describes the node change event. The 
	 * value does not differentiate between when the event occurred (before or after).
	 * You can create separate event handlers to handle both instances.
	 */
	var Action(default, never):cs.system.xml.XmlNodeChangedAction;
	/**
	 * Gets the value of the  after the operation completes.
	 * @return The value of the  after the operation completes. This property returns 
	 * if the node is being removed. For attribute nodes this property returns the .
	 */
	var NewParent(default, never):cs.system.xml.XmlNode;
	/**
	 * Gets the new value of the node.
	 * @return The new value of the node. This property returns  if the node is neither
	 * an attribute nor a text node, or if the node is being removed. If called in a 
	 * event,  returns the value of the node if the change is successful. If called in
	 * a  event,  returns the current value of the node.
	 */
	var NewValue(default, never):String;
	/**
	 * Gets the  that is being added, removed or changed.
	 * @return The  that is being added, removed or changed; this property never
	 * returns .
	 */
	var Node(default, never):cs.system.xml.XmlNode;
	/**
	 * Gets the value of the  before the operation began.
	 * @return The value of the  before the operation began. This property returns  if
	 * the node did not have a parent. For attribute nodes this property returns the .
	 */
	var OldParent(default, never):cs.system.xml.XmlNode;
	/**
	 * Gets the original value of the node.
	 * @return The original value of the node. This property returns  if the node is
	 * neither an attribute nor a text node, or if the node is being inserted. If
	 * called in a  event,  returns the current value of the node that will be replaced
	 * if the change is successful. If called in a  event,  returns the value of node
	 * prior to the change.
	 */
	var OldValue(default, never):String;
	function new(node:cs.system.xml.XmlNode, oldParent:cs.system.xml.XmlNode, newParent:cs.system.xml.XmlNode, oldValue:String, newValue:String, action:cs.system.xml.XmlNodeChangedAction):Void;
}
