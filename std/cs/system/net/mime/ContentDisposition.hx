package cs.system.net.mime;

/** Represents a MIME protocol Content-Disposition header. */
@:native("System.Net.Mime.ContentDisposition")
extern class ContentDisposition {
	/**
	 * Gets or sets the creation date for a file attachment.
	 * @return A  value that indicates the file creation date; otherwise,  if no date
	 * was specified.
	 */
	var CreationDate(default, default):cs.system.DateTime;
	/**
	 * Gets or sets the disposition type for an email attachment.
	 * @return A  that contains the disposition type. The value is not restricted but
	 * is typically one of the  values.
	 */
	var DispositionType(default, default):String;
	/**
	 * Gets or sets the suggested file name for an email attachment.
	 * @return A  that contains the file name.
	 */
	var FileName(default, default):String;
	/**
	 * Gets or sets a  value that determines the disposition type (Inline or
	 * Attachment) for an email attachment.
	 * @return if content in the attachment is presented inline as part of the email
	 * body; otherwise, .
	 */
	var Inline(default, default):Bool;
	/**
	 * Gets or sets the modification date for a file attachment.
	 * @return A  value that indicates the file modification date; otherwise,  if no
	 * date was specified.
	 */
	var ModificationDate(default, default):cs.system.DateTime;
	/**
	 * Gets the parameters included in the Content-Disposition header represented by
	 * this instance.
	 * @return A writable  that contains parameter name/value pairs.
	 */
	var Parameters(default, never):cs.system.collections.specialized.StringDictionary;
	/**
	 * Gets or sets the read date for a file attachment.
	 * @return A  value that indicates the file read date; otherwise,  if no date was
	 * specified.
	 */
	var ReadDate(default, default):cs.system.DateTime;
	/**
	 * Gets or sets the size of a file attachment.
	 * @return A  that specifies the number of bytes in the file attachment. The
	 * default value is -1, which indicates that the file size is unknown.
	 */
	var Size(default, default):haxe.Int64;
	@:overload(function():Void {})
	function new(disposition:String):Void;
	/**
	 * Determines whether the content-disposition header of the specified  object is
	 * equal to the content-disposition header of this object.
	 * @param rparam The  object to compare with this object.
	 * @return if the content-disposition headers are the same; otherwise .
	 */
	function Equals(rparam:Dynamic):Bool;
	/**
	 * Determines the hash code of the specified  object
	 * @return An integer hash value.
	 */
	function GetHashCode():Int;
	/**
	 * Returns a  representation of this instance.
	 * @return A  that contains the property values for this instance.
	 */
	function ToString():String;
}
