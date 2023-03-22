package asys.native.system;

/**
	Possible actions to handle inter-process signals.
**/
enum SignalAction {
	/**
		Ignore a signal.
	**/
	Ignore;

	/**
		Use default action for a signal.
	**/
	Default;

	/**
		Execute `handler` on a signal receipt.
	**/
	Handle(handler:() -> Void);
}