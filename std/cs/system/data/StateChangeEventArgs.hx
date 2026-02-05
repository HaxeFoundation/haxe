package cs.system.data;

/** Provides data for the state change event of a .NET Framework data provider. */
@:native("System.Data.StateChangeEventArgs")
extern class StateChangeEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the new state of the connection. The connection object will be in the new
	 * state already when the event is fired.
	 * @return One of the  values.
	 */
	var CurrentState(default, never):cs.system.data.ConnectionState;
	/**
	 * Gets the original state of the connection.
	 * @return One of the  values.
	 */
	var OriginalState(default, never):cs.system.data.ConnectionState;
	function new(originalState:cs.system.data.ConnectionState, currentState:cs.system.data.ConnectionState):Void;
}
