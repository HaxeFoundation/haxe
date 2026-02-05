package cs.system.diagnostics;

/** Specifies the reason a thread is waiting. */
@:native("System.Diagnostics.ThreadWaitReason")
extern enum abstract ThreadWaitReason(Int) {
	var EventPairHigh = 7;
	var EventPairLow = 8;
	var ExecutionDelay = 4;
	var Executive = 0;
	var FreePage = 1;
	var LpcReceive = 9;
	var LpcReply = 10;
	var PageIn = 2;
	var PageOut = 12;
	var Suspended = 5;
	var SystemAllocation = 3;
	var Unknown = 13;
	var UserRequest = 6;
	var VirtualMemory = 11;
}
