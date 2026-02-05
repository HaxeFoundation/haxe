package cs.system.net.mail;

/** Represents an attachment to an email. */
@:native("System.Net.Mail.Attachment")
extern class Attachment extends cs.system.net.mail.AttachmentBase {
	/**
	 * Gets the MIME content disposition for this attachment.
	 * @return A  that provides the presentation information for this attachment.
	 */
	var ContentDisposition(default, never):cs.system.net.mime.ContentDisposition;
	/**
	 * Gets or sets the MIME content type name value in the content type associated
	 * with this attachment.
	 * @return A  that contains the value for the content type  represented by the 
	 * property.
	 */
	var Name(default, default):String;
	/**
	 * Specifies the encoding for the .
	 * @return An  value that specifies the type of name encoding. The default value is
	 * determined from the name of the attachment.
	 */
	var NameEncoding(default, default):cs.system.text.Encoding;
	@:overload(function(fileName:String):Void {})
	@:overload(function(contentStream:cs.system.io.Stream, contentType:cs.system.net.mime.ContentType):Void {})
	@:overload(function(contentStream:cs.system.io.Stream, name:String):Void {})
	@:overload(function(fileName:String, contentType:cs.system.net.mime.ContentType):Void {})
	@:overload(function(fileName:String, mediaType:String):Void {})
	function new(contentStream:cs.system.io.Stream, name:String, mediaType:String):Void;
	@:overload(function(content:String, contentType:cs.system.net.mime.ContentType):cs.system.net.mail.Attachment {})
	@:overload(function(content:String, name:String):cs.system.net.mail.Attachment {})
	/**
	 * Creates a mail attachment using the content from the specified string, and the
	 * specified .
	 * @param content A  that contains the content for this attachment.
	 * @param contentType A  object that represents the Multipurpose Internet Mail
	 * Exchange (MIME) protocol Content-Type header to be used.
	 * @return An object of type .
	 */
	static function CreateAttachmentFromString(content:String, name:String, contentEncoding:cs.system.text.Encoding, mediaType:String):cs.system.net.mail.Attachment;
}
