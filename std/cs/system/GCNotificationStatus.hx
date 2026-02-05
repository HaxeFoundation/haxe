package cs.system;

/** Provides information about the current registration for notification of the next full garbage collection. */
@:native("System.GCNotificationStatus")
extern enum GCNotificationStatus {
	Canceled;
	Failed;
	NotApplicable;
	Succeeded;
	Timeout;
}
