package cs.system.transactions;

/** Specifies how distributed transactions interact with COM+ transactions. */
@:native("System.Transactions.EnterpriseServicesInteropOption")
extern enum EnterpriseServicesInteropOption {
	Automatic;
	Full;
	None;
}
