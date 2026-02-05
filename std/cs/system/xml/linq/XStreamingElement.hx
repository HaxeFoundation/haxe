package cs.system.xml.linq;

/** Represents elements in an XML tree that supports deferred streaming output. */
@:native("System.Xml.Linq.XStreamingElement")
extern class XStreamingElement {
	/**
	 * Gets or sets the name of this streaming element.
	 * @return An  that contains the name of this streaming element.
	 */
	var Name(default, default):cs.system.xml.linq.XName;
	@:overload(function(name:cs.system.xml.linq.XName):Void {})
	@:overload(function(name:cs.system.xml.linq.XName, content:Dynamic):Void {})
	function new(name:cs.system.xml.linq.XName, content:cs.NativeArray<Dynamic>):Void;
	@:overload(function(content:Dynamic):Void {})
	/**
	 * Adds the specified content as children to this .
	 * @param content Content to be added to the streaming element.
	 */
	function Add(content:cs.NativeArray<Dynamic>):Void;
	@:overload(function(stream:cs.system.io.Stream):Void {})
	@:overload(function(textWriter:cs.system.io.TextWriter):Void {})
	@:overload(function(fileName:String):Void {})
	@:overload(function(writer:cs.system.xml.XmlWriter):Void {})
	@:overload(function(stream:cs.system.io.Stream, options:cs.system.xml.linq.SaveOptions):Void {})
	@:overload(function(textWriter:cs.system.io.TextWriter, options:cs.system.xml.linq.SaveOptions):Void {})
	/**
	 * Outputs this  to the specified .
	 * @param stream The stream to output this  to.
	 */
	function Save(fileName:String, options:cs.system.xml.linq.SaveOptions):Void;
	@:overload(function():String {})
	/**
	 * Returns the formatted (indented) XML for this streaming element.
	 * @return A  containing the indented XML.
	 */
	function ToString(options:cs.system.xml.linq.SaveOptions):String;
	/**
	 * Writes this streaming element to an .
	 * @param writer An  into which this method will write.
	 */
	function WriteTo(writer:cs.system.xml.XmlWriter):Void;
}
