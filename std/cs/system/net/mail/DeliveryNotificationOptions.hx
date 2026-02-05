package cs.system.net.mail;

/** Describes the delivery notification options for email. */
@:native("System.Net.Mail.DeliveryNotificationOptions")
extern enum abstract DeliveryNotificationOptions(Int) {
	var Delay = 4;
	var Never = 134217728;
	var None = 0;
	var OnFailure = 2;
	var OnSuccess = 1;
	@:op(A | B) static function or(lhs:DeliveryNotificationOptions, rhs:DeliveryNotificationOptions):DeliveryNotificationOptions;
	@:op(A & B) static function and(lhs:DeliveryNotificationOptions, rhs:DeliveryNotificationOptions):DeliveryNotificationOptions;
	@:op(A ^ B) static function xor(lhs:DeliveryNotificationOptions, rhs:DeliveryNotificationOptions):DeliveryNotificationOptions;
	@:op(~A) static function complement(value:DeliveryNotificationOptions):DeliveryNotificationOptions;
}
