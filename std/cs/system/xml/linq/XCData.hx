package cs.system.xml.linq;

/** Represents a text node that contains CDATA. */
@:native("System.Xml.Linq.XCData")
extern class XCData extends cs.system.xml.linq.XText {
	@:overload(function(value:String):Void {})
	function new(other:cs.system.xml.linq.XCData):Void;
	/**
	 * Writes this CDATA object to an .
	 * @param writer An  into which this method will write.
	 */
	function WriteTo(writer:cs.system.xml.XmlWriter):Void;
	/**
	 * @param writer 
	 * @param cancellationToken 
	 */
	function WriteToAsync(writer:cs.system.xml.XmlWriter, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
}
