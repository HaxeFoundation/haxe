package cs.system.net.mail;

/** Represents a collection of  objects. */
@:native("System.Net.Mail.AlternateViewCollection")
extern class AlternateViewCollection extends cs.system.collections.objectmodel.Collection<cs.system.net.mail.AlternateView> {
	/** Releases all resources used by the . */
	function Dispose():Void;
}
