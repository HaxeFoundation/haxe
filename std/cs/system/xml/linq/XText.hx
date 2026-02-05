package cs.system.xml.linq;

/** Represents a text node. */
@:native("System.Xml.Linq.XText")
extern class XText extends cs.system.xml.linq.XNode {
	/**
	 * Gets or sets the value of this node.
	 * @return A  that contains the value of this node.
	 */
	var Value(default, default):String;
	@:overload(function(value:String):Void {})
	function new(other:cs.system.xml.linq.XText):Void;
	/**
	 * Writes this node to an .
	 * @param writer An  into which this method will write.
	 */
	function WriteTo(writer:cs.system.xml.XmlWriter):Void;
	/**
	 * @param writer 
	 * @param cancellationToken 
	 */
	function WriteToAsync(writer:cs.system.xml.XmlWriter, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
}
