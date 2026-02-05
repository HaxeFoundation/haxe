package cs.system.runtime.interopservices;

/** Enables developers to provide a custom, managed implementation of the IUnknown::QueryInterface(REFIID riid, void **ppvObject) method. */
@:native("System.Runtime.InteropServices.ICustomQueryInterface")
extern interface ICustomQueryInterface {
	/**
	 * Returns an interface according to a specified interface ID.
	 * @param iid The GUID of the requested interface.
	 * @param ppv A reference to the requested interface, when this method returns.
	 * @return One of the enumeration values that indicates whether a custom
	 * implementation of IUnknown::QueryInterface was used.
	 */
	function GetInterface(iid:cs.Ref<cs.system.Guid>, ppv:cs.Ref<cs.system.IntPtr>):cs.system.runtime.interopservices.CustomQueryInterfaceResult;
}
