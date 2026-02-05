package cs.system.net.mail;

/** Specifies how email messages are delivered. */
@:native("System.Net.Mail.SmtpDeliveryMethod")
extern enum abstract SmtpDeliveryMethod(Int) {
	var Network = 0;
	var PickupDirectoryFromIis = 2;
	var SpecifiedPickupDirectory = 1;
}
