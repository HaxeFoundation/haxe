package cs.system.xml;

/** Defines the context for a set of  objects. */
@:native("System.Xml.XmlImplementation")
extern class XmlImplementation {
	@:overload(function():Void {})
	function new(nt:cs.system.xml.XmlNameTable):Void;
	/**
	 * Creates a new .
	 * @return The new  object.
	 */
	function CreateDocument():cs.system.xml.XmlDocument;
	/**
	 * Tests if the Document Object Model (DOM) implementation implements a specific
	 * feature.
	 * @param strFeature The package name of the feature to test. This name is not
	 * case-sensitive.
	 * @param strVersion This is the version number of the package name to test. If the
	 * version is not specified (), supporting any version of the feature causes the
	 * method to return .
	 * @return if the feature is implemented in the specified version; otherwise, . The
	 * following table shows the combinations that cause  to return . strFeature
	 * strVersion XML 1.0 XML 2.0
	 */
	function HasFeature(strFeature:String, strVersion:String):Bool;
}
