package cs.system.runtime.compilerservices;

@:native("System.Runtime.CompilerServices.ConfiguredCancelableAsyncEnumerable`1.Enumerator")
extern class ConfiguredCancelableAsyncEnumerable_Enumerator<T> extends cs.system.ValueType {
	var Current(default, never):T;
	function DisposeAsync():cs.system.runtime.compilerservices.ConfiguredValueTaskAwaitable;
	function MoveNextAsync():cs.system.runtime.compilerservices.ConfiguredValueTaskAwaitable_1<Bool>;
}
