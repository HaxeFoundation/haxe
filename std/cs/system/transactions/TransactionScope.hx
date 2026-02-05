package cs.system.transactions;

/** Makes a code block transactional. This class cannot be inherited. */
@:native("System.Transactions.TransactionScope")
extern class TransactionScope {
	@:overload(function():Void {})
	@:overload(function(transactionToUse:cs.system.transactions.Transaction):Void {})
	@:overload(function(asyncFlowOption:cs.system.transactions.TransactionScopeAsyncFlowOption):Void {})
	@:overload(function(scopeOption:cs.system.transactions.TransactionScopeOption):Void {})
	@:overload(function(transactionToUse:cs.system.transactions.Transaction, scopeTimeout:cs.system.TimeSpan):Void {})
	@:overload(function(transactionToUse:cs.system.transactions.Transaction, asyncFlowOption:cs.system.transactions.TransactionScopeAsyncFlowOption):Void {})
	@:overload(function(scopeOption:cs.system.transactions.TransactionScopeOption, scopeTimeout:cs.system.TimeSpan):Void {})
	@:overload(function(scopeOption:cs.system.transactions.TransactionScopeOption, transactionOptions:cs.system.transactions.TransactionOptions):Void {})
	@:overload(function(scopeOption:cs.system.transactions.TransactionScopeOption, asyncFlowOption:cs.system.transactions.TransactionScopeAsyncFlowOption):Void {})
	@:overload(function(transactionToUse:cs.system.transactions.Transaction, scopeTimeout:cs.system.TimeSpan, interopOption:cs.system.transactions.EnterpriseServicesInteropOption):Void {})
	@:overload(function(transactionToUse:cs.system.transactions.Transaction, scopeTimeout:cs.system.TimeSpan, asyncFlowOption:cs.system.transactions.TransactionScopeAsyncFlowOption):Void {})
	@:overload(function(scopeOption:cs.system.transactions.TransactionScopeOption, scopeTimeout:cs.system.TimeSpan, asyncFlowOption:cs.system.transactions.TransactionScopeAsyncFlowOption):Void {})
	@:overload(function(scopeOption:cs.system.transactions.TransactionScopeOption, transactionOptions:cs.system.transactions.TransactionOptions, interopOption:cs.system.transactions.EnterpriseServicesInteropOption):Void {})
	function new(scopeOption:cs.system.transactions.TransactionScopeOption, transactionOptions:cs.system.transactions.TransactionOptions, asyncFlowOption:cs.system.transactions.TransactionScopeAsyncFlowOption):Void;
	/** Indicates that all operations within the scope are completed successfully. */
	function Complete():Void;
	/** Ends the transaction scope. */
	function Dispose():Void;
}
