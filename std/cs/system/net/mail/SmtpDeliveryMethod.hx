package cs.system.net.mail;

/** Specifies how email messages are delivered. */
@:native("System.Net.Mail.SmtpDeliveryMethod")
extern enum SmtpDeliveryMethod {
	Network;
	PickupDirectoryFromIis;
	SpecifiedPickupDirectory;
}
