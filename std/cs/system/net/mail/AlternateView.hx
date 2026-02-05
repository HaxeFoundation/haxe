package cs.system.net.mail;

/** Represents the format to view an email message. */
@:native("System.Net.Mail.AlternateView")
extern class AlternateView extends cs.system.net.mail.AttachmentBase {
	/**
	 * Gets or sets the base URI to use for resolving relative URIs in the .
	 * @return The base URI to use for resolving relative URIs in the .
	 */
	var BaseUri(default, default):cs.system.Uri;
	/**
	 * Gets the set of embedded resources referred to by this attachment.
	 * @return A  object that stores the collection of linked resources to be sent as
	 * part of an email message.
	 */
	var LinkedResources(default, never):cs.system.net.mail.LinkedResourceCollection;
	@:overload(function(contentStream:cs.system.io.Stream):Void {})
	@:overload(function(fileName:String):Void {})
	@:overload(function(contentStream:cs.system.io.Stream, contentType:cs.system.net.mime.ContentType):Void {})
	@:overload(function(contentStream:cs.system.io.Stream, mediaType:String):Void {})
	@:overload(function(fileName:String, contentType:cs.system.net.mime.ContentType):Void {})
	function new(fileName:String, mediaType:String):Void;
	@:overload(function(content:String):cs.system.net.mail.AlternateView {})
	@:overload(function(content:String, contentType:cs.system.net.mime.ContentType):cs.system.net.mail.AlternateView {})
	/**
	 * Creates a  of an email message using the content specified in a .
	 * @param content The  that contains the content of the email message.
	 * @return An  object that represents an alternate view of an email message.
	 */
	static function CreateAlternateViewFromString(content:String, contentEncoding:cs.system.text.Encoding, mediaType:String):cs.system.net.mail.AlternateView;
}
