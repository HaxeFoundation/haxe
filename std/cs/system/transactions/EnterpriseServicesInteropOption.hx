package cs.system.transactions;

/** Specifies how distributed transactions interact with COM+ transactions. */
@:native("System.Transactions.EnterpriseServicesInteropOption")
extern enum abstract EnterpriseServicesInteropOption(Int) {
	var Automatic = 1;
	var Full = 2;
	var None = 0;
}
