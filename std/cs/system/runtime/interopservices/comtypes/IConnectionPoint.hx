package cs.system.runtime.interopservices.comtypes;

/** Provides the managed definition of the  interface. */
@:native("System.Runtime.InteropServices.ComTypes.IConnectionPoint")
extern interface IConnectionPoint {
	/**
	 * Establishes an advisory connection between the connection point and the caller's
	 * sink object.
	 * @param pUnkSink A reference to the sink to receive calls for the outgoing
	 * interface managed by this connection point.
	 * @param pdwCookie When this method returns, contains the connection cookie. This
	 * parameter is passed uninitialized.
	 */
	function Advise(pUnkSink:Dynamic, pdwCookie:cs.Ref<Int>):Void;
	/**
	 * Creates an enumerator object for iteration through the connections that exist to
	 * this connection point.
	 * @param ppEnum When this method returns, contains the newly created enumerator.
	 * This parameter is passed uninitialized.
	 */
	function EnumConnections(ppEnum:cs.Ref<cs.system.runtime.interopservices.comtypes.IEnumConnections>):Void;
	/**
	 * Returns the IID of the outgoing interface managed by this connection point.
	 * @param pIID When this parameter returns, contains the IID of the outgoing
	 * interface managed by this connection point. This parameter is passed
	 * uninitialized.
	 */
	function GetConnectionInterface(pIID:cs.Ref<cs.system.Guid>):Void;
	/**
	 * Retrieves the  interface pointer to the connectable object that conceptually
	 * owns this connection point.
	 * @param ppCPC When this parameter returns, contains the connectable object's 
	 * interface. This parameter is passed uninitialized.
	 */
	function GetConnectionPointContainer(ppCPC:cs.Ref<cs.system.runtime.interopservices.comtypes.IConnectionPointContainer>):Void;
	/**
	 * Terminates an advisory connection previously established through the  method.
	 * @param dwCookie The connection cookie previously returned from the  method.
	 */
	function Unadvise(dwCookie:Int):Void;
}
