package cs.system.diagnostics;

/** Specifies the reason a thread is waiting. */
@:native("System.Diagnostics.ThreadWaitReason")
extern enum ThreadWaitReason {
	EventPairHigh;
	EventPairLow;
	ExecutionDelay;
	Executive;
	FreePage;
	LpcReceive;
	LpcReply;
	PageIn;
	PageOut;
	Suspended;
	SystemAllocation;
	Unknown;
	UserRequest;
	VirtualMemory;
}
