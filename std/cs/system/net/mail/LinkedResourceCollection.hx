package cs.system.net.mail;

/** Stores linked resources to be sent as part of an email message. */
@:native("System.Net.Mail.LinkedResourceCollection")
extern class LinkedResourceCollection extends cs.system.collections.objectmodel.Collection<cs.system.net.mail.LinkedResource> {
	/** Releases all resources used by the . */
	function Dispose():Void;
}
