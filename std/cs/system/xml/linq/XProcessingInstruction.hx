package cs.system.xml.linq;

/** Represents an XML processing instruction. */
@:native("System.Xml.Linq.XProcessingInstruction")
extern class XProcessingInstruction extends cs.system.xml.linq.XNode {
	/**
	 * Gets or sets the string value of this processing instruction.
	 * @return A  that contains the string value of this processing instruction.
	 */
	var Data(default, default):String;
	/**
	 * Gets or sets a string containing the target application for this processing
	 * instruction.
	 * @return A  containing the target application for this processing instruction.
	 */
	var Target(default, default):String;
	@:overload(function(other:cs.system.xml.linq.XProcessingInstruction):Void {})
	function new(target:String, data:String):Void;
	/**
	 * Writes this processing instruction to an .
	 * @param writer The  to write this processing instruction to.
	 */
	function WriteTo(writer:cs.system.xml.XmlWriter):Void;
	/**
	 * @param writer 
	 * @param cancellationToken 
	 */
	function WriteToAsync(writer:cs.system.xml.XmlWriter, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
}
