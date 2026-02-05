package cs.system.xml.linq;

/** Represents an XML Document Type Definition (DTD). */
@:native("System.Xml.Linq.XDocumentType")
extern class XDocumentType extends cs.system.xml.linq.XNode {
	/**
	 * Gets or sets the internal subset for this Document Type Definition (DTD).
	 * @return A  that contains the internal subset for this Document Type Definition
	 * (DTD).
	 */
	var InternalSubset(default, default):String;
	/**
	 * Gets or sets the name for this Document Type Definition (DTD).
	 * @return A  that contains the name for this Document Type Definition (DTD).
	 */
	var Name(default, default):String;
	/**
	 * Gets or sets the public identifier for this Document Type Definition (DTD).
	 * @return A  that contains the public identifier for this Document Type Definition
	 * (DTD).
	 */
	var PublicId(default, default):String;
	/**
	 * Gets or sets the system identifier for this Document Type Definition (DTD).
	 * @return A  that contains the system identifier for this Document Type Definition
	 * (DTD).
	 */
	var SystemId(default, default):String;
	@:overload(function(other:cs.system.xml.linq.XDocumentType):Void {})
	function new(name:String, publicId:String, systemId:String, internalSubset:String):Void;
	/**
	 * Write this  to an .
	 * @param writer An  into which this method will write.
	 */
	function WriteTo(writer:cs.system.xml.XmlWriter):Void;
	/**
	 * @param writer 
	 * @param cancellationToken 
	 */
	function WriteToAsync(writer:cs.system.xml.XmlWriter, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
}
