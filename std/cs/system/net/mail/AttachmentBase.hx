package cs.system.net.mail;

/** Base class that represents an email attachment. Classes , , and  derive from this class. */
@:native("System.Net.Mail.AttachmentBase")
extern class AttachmentBase {
	/**
	 * Gets or sets the MIME content ID for this attachment.
	 * @return A  holding the content ID.
	 */
	var ContentId(default, default):String;
	/**
	 * Gets the content stream of this attachment.
	 * @return The content stream of this attachment.
	 */
	var ContentStream(default, never):cs.system.io.Stream;
	/**
	 * Gets the content type of this attachment.
	 * @return The content type for this attachment.
	 */
	var ContentType(default, default):cs.system.net.mime.ContentType;
	/**
	 * Gets or sets the encoding of this attachment.
	 * @return The encoding for this attachment.
	 */
	var TransferEncoding(default, default):cs.system.net.mime.TransferEncoding;
	/** Releases the resources used by the . */
	function Dispose():Void;
}
