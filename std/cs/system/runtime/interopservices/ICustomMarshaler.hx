package cs.system.runtime.interopservices;

/** Provides custom wrappers for handling method calls. */
@:native("System.Runtime.InteropServices.ICustomMarshaler")
extern interface ICustomMarshaler {
	/**
	 * Performs necessary cleanup of the managed data when it is no longer needed.
	 * @param ManagedObj The managed object to be destroyed.
	 */
	function CleanUpManagedData(ManagedObj:Dynamic):Void;
	/**
	 * Performs necessary cleanup of the unmanaged data when it is no longer needed.
	 * @param pNativeData A pointer to the unmanaged data to be destroyed.
	 */
	function CleanUpNativeData(pNativeData:cs.system.IntPtr):Void;
	/**
	 * Returns the size of the native data to be marshaled.
	 * @return The size, in bytes, of the native data.
	 */
	function GetNativeDataSize():Int;
	/**
	 * Converts the managed data to unmanaged data.
	 * @param ManagedObj The managed object to be converted.
	 * @return A pointer to the COM view of the managed object.
	 */
	function MarshalManagedToNative(ManagedObj:Dynamic):cs.system.IntPtr;
	/**
	 * Converts the unmanaged data to managed data.
	 * @param pNativeData A pointer to the unmanaged data to be wrapped.
	 * @return An object that represents the managed view of the COM data.
	 */
	function MarshalNativeToManaged(pNativeData:cs.system.IntPtr):Dynamic;
}
