package cs.system.threading;

@:native("System.Threading.AsyncLocalValueChangedArgs")
extern class AsyncLocalValueChangedArgs<T> extends cs.system.ValueType {
	var CurrentValue(default, never):T;
	var PreviousValue(default, never):T;
	var ThreadContextChanged(default, never):Bool;
}
