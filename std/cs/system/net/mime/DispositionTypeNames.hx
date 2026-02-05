package cs.system.net.mime;

/** Supplies the strings used to specify the disposition type for an email attachment. */
@:native("System.Net.Mime.DispositionTypeNames")
extern class DispositionTypeNames {
	/** Specifies that the attachment is to be displayed as a file attached to the email message. */
	static var Attachment(default, never):String;
	/** Specifies that the attachment is to be displayed as part of the email message body. */
	static var Inline(default, never):String;
}
