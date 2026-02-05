package cs.system.runtime.interopservices;

/** Enables users to write activation code for managed objects that extend . */
@:native("System.Runtime.InteropServices.ICustomFactory")
extern interface ICustomFactory {
	/**
	 * Creates a new instance of the specified type.
	 * @param serverType The type to activate.
	 * @return A  associated with the specified type.
	 */
	function CreateInstance(serverType:cs.system.Type):cs.system.MarshalByRefObject;
}
