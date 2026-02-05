package cs.system.xml.linq;

/** Represents an XML document. For the components and usage of an  object, see XDocument Class Overview. */
@:native("System.Xml.Linq.XDocument")
extern class XDocument extends cs.system.xml.linq.XContainer {
	/**
	 * Gets or sets the XML declaration for this document.
	 * @return An  that contains the XML declaration for this document.
	 */
	var Declaration(default, default):cs.system.xml.linq.XDeclaration;
	/**
	 * Gets the Document Type Definition (DTD) for this document.
	 * @return A  that contains the DTD for this document.
	 */
	var DocumentType(default, never):cs.system.xml.linq.XDocumentType;
	/**
	 * Gets the root element of the XML Tree for this document.
	 * @return The root  of the XML tree.
	 */
	var Root(default, never):cs.system.xml.linq.XElement;
	@:overload(function():Void {})
	@:overload(function(content:cs.NativeArray<Dynamic>):Void {})
	@:overload(function(other:cs.system.xml.linq.XDocument):Void {})
	function new(declaration:cs.system.xml.linq.XDeclaration, content:cs.NativeArray<Dynamic>):Void;
	@:overload(function(stream:cs.system.io.Stream):cs.system.xml.linq.XDocument {})
	@:overload(function(textReader:cs.system.io.TextReader):cs.system.xml.linq.XDocument {})
	@:overload(function(uri:String):cs.system.xml.linq.XDocument {})
	@:overload(function(reader:cs.system.xml.XmlReader):cs.system.xml.linq.XDocument {})
	@:overload(function(stream:cs.system.io.Stream, options:cs.system.xml.linq.LoadOptions):cs.system.xml.linq.XDocument {})
	@:overload(function(textReader:cs.system.io.TextReader, options:cs.system.xml.linq.LoadOptions):cs.system.xml.linq.XDocument {})
	@:overload(function(uri:String, options:cs.system.xml.linq.LoadOptions):cs.system.xml.linq.XDocument {})
	/**
	 * Creates a new  instance by using the specified stream.
	 * @param stream The stream that contains the XML data.
	 * @return An  object that reads the data that is contained in the stream.
	 */
	static function Load(reader:cs.system.xml.XmlReader, options:cs.system.xml.linq.LoadOptions):cs.system.xml.linq.XDocument;
	@:overload(function(stream:cs.system.io.Stream, options:cs.system.xml.linq.LoadOptions, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.xml.linq.XDocument> {})
	@:overload(function(textReader:cs.system.io.TextReader, options:cs.system.xml.linq.LoadOptions, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.xml.linq.XDocument> {})
	/**
	 * @param stream 
	 * @param options 
	 * @param cancellationToken 
	 */
	static function LoadAsync(reader:cs.system.xml.XmlReader, options:cs.system.xml.linq.LoadOptions, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.xml.linq.XDocument>;
	@:overload(function(text:String):cs.system.xml.linq.XDocument {})
	/**
	 * Creates a new  from a string.
	 * @param text A string that contains XML.
	 * @return An  populated from the string that contains XML.
	 */
	static function Parse(text:String, options:cs.system.xml.linq.LoadOptions):cs.system.xml.linq.XDocument;
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
	@:overload(function(writer:cs.system.xml.XmlWriter, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	@:overload(function(stream:cs.system.io.Stream, options:cs.system.xml.linq.SaveOptions, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	/**
	 * @param stream 
	 * @param options 
	 * @param cancellationToken 
	 */
	function SaveAsync(textWriter:cs.system.io.TextWriter, options:cs.system.xml.linq.SaveOptions, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/**
	 * Write this document to an .
	 * @param writer An  into which this method will write.
	 */
	function WriteTo(writer:cs.system.xml.XmlWriter):Void;
	/**
	 * @param writer 
	 * @param cancellationToken 
	 */
	function WriteToAsync(writer:cs.system.xml.XmlWriter, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
}
