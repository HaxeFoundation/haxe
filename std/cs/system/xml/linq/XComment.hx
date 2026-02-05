package cs.system.xml.linq;

/** Represents an XML comment. */
@:native("System.Xml.Linq.XComment")
extern class XComment extends cs.system.xml.linq.XNode {
	/**
	 * Gets or sets the string value of this comment.
	 * @return A  that contains the string value of this comment.
	 */
	var Value(default, default):String;
	@:overload(function(value:String):Void {})
	function new(other:cs.system.xml.linq.XComment):Void;
	/**
	 * Write this comment to an .
	 * @param writer An  into which this method will write.
	 */
	function WriteTo(writer:cs.system.xml.XmlWriter):Void;
	/**
	 * @param writer 
	 * @param cancellationToken 
	 */
	function WriteToAsync(writer:cs.system.xml.XmlWriter, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
}
