package cs.system;

/** Provides information about the current registration for notification of the next full garbage collection. */
@:native("System.GCNotificationStatus")
extern enum abstract GCNotificationStatus(Int) {
	var Canceled = 2;
	var Failed = 1;
	var NotApplicable = 4;
	var Succeeded = 0;
	var Timeout = 3;
}
