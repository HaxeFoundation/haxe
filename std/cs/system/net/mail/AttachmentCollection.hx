package cs.system.net.mail;

/** Stores attachments to be sent as part of an email message. */
@:native("System.Net.Mail.AttachmentCollection")
extern class AttachmentCollection extends cs.system.collections.objectmodel.Collection<cs.system.net.mail.Attachment> {
	/** Releases all resources used by the . */
	function Dispose():Void;
}
