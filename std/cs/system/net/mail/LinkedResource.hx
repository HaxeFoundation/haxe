package cs.system.net.mail;

/** Represents an embedded external resource in an email attachment, such as an image in an HTML attachment. */
@:native("System.Net.Mail.LinkedResource")
extern class LinkedResource extends cs.system.net.mail.AttachmentBase {
	/**
	 * Gets or sets a URI that the resource must match.
	 * @return If  is a relative URI, the recipient of the message must resolve it.
	 */
	var ContentLink(default, default):cs.system.Uri;
	@:overload(function(contentStream:cs.system.io.Stream):Void {})
	@:overload(function(fileName:String):Void {})
	@:overload(function(contentStream:cs.system.io.Stream, contentType:cs.system.net.mime.ContentType):Void {})
	@:overload(function(contentStream:cs.system.io.Stream, mediaType:String):Void {})
	@:overload(function(fileName:String, contentType:cs.system.net.mime.ContentType):Void {})
	function new(fileName:String, mediaType:String):Void;
	@:overload(function(content:String):cs.system.net.mail.LinkedResource {})
	@:overload(function(content:String, contentType:cs.system.net.mime.ContentType):cs.system.net.mail.LinkedResource {})
	/**
	 * Creates a  object from a string to be included in an email attachment as an
	 * embedded resource. The default media type is plain text, and the default content
	 * type is ASCII.
	 * @param content A string that contains the embedded resource to be included in
	 * the email attachment.
	 * @return A  object that contains the embedded resource to be included in the
	 * email attachment.
	 */
	static function CreateLinkedResourceFromString(content:String, contentEncoding:cs.system.text.Encoding, mediaType:String):cs.system.net.mail.LinkedResource;
}
