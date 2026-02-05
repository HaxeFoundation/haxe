package cs.system.net.mail;

/** Specifies the priority of a . */
@:native("System.Net.Mail.MailPriority")
extern enum abstract MailPriority(Int) {
	var High = 2;
	var Low = 1;
	var Normal = 0;
}
