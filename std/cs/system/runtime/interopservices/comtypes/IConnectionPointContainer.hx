package cs.system.runtime.interopservices.comtypes;

/** Provides the managed definition of the  interface. */
@:native("System.Runtime.InteropServices.ComTypes.IConnectionPointContainer")
extern interface IConnectionPointContainer {
	/**
	 * Creates an enumerator of all the connection points supported in the connectable
	 * object, one connection point per IID.
	 * @param ppEnum When this method returns, contains the interface pointer of the
	 * enumerator. This parameter is passed uninitialized.
	 */
	function EnumConnectionPoints(ppEnum:cs.Ref<cs.system.runtime.interopservices.comtypes.IEnumConnectionPoints>):Void;
	/**
	 * Asks the connectable object if it has a connection point for a particular IID,
	 * and if so, returns the  interface pointer to that connection point.
	 * @param riid A reference to the outgoing interface IID whose connection point is
	 * being requested.
	 * @param ppCP When this method returns, contains the connection point that manages
	 * the outgoing interface . This parameter is passed uninitialized.
	 */
	function FindConnectionPoint(riid:cs.Ref<cs.system.Guid>, ppCP:cs.Ref<cs.system.runtime.interopservices.comtypes.IConnectionPoint>):Void;
}
