package cs.system.xml;

/** Contains configurable quota values for XmlDictionaryReaders. */
@:native("System.Xml.XmlDictionaryReaderQuotas")
extern class XmlDictionaryReaderQuotas {
	/**
	 * Gets an instance of this class with all properties set to maximum values.
	 * @return An instance of  with properties set to .
	 */
	static var Max(default, never):cs.system.xml.XmlDictionaryReaderQuotas;
	/**
	 * Gets or sets the maximum allowed array length.
	 * @return The maximum allowed array length. The default is 16384.
	 */
	var MaxArrayLength(default, default):Int;
	/**
	 * Gets or sets the maximum allowed bytes returned for each read.
	 * @return The maximum allowed bytes returned for each read. The default is 4096.
	 */
	var MaxBytesPerRead(default, default):Int;
	/**
	 * Gets or sets the maximum nested node depth.
	 * @return The maximum nested node depth. The default is 32;
	 */
	var MaxDepth(default, default):Int;
	/**
	 * Gets or sets the maximum characters allowed in a table name.
	 * @return The maximum characters allowed in a table name. The default is 16384.
	 */
	var MaxNameTableCharCount(default, default):Int;
	/**
	 * Gets or sets the maximum string length returned by the reader.
	 * @return The maximum string length returned by the reader. The default is 8192.
	 */
	var MaxStringContentLength(default, default):Int;
	/**
	 * Gets the modified quotas for the .
	 * @return The modified quotas for the .
	 */
	var ModifiedQuotas(default, never):cs.system.xml.XmlDictionaryReaderQuotaTypes;
	function new():Void;
	/**
	 * Sets the properties on a passed-in quotas instance, based on the values in this
	 * instance.
	 * @param quotas The  instance to which to copy values.
	 */
	function CopyTo(quotas:cs.system.xml.XmlDictionaryReaderQuotas):Void;
}
