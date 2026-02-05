package cs.system.runtime.interopservices.comtypes;

/** Provides a managed definition of the  interface. */
@:native("System.Runtime.InteropServices.ComTypes.IAdviseSink")
extern interface IAdviseSink {
	/** Notifies all registered advisory sinks that the object has changed from the running state to the loaded state.  This method is called by a server. */
	function OnClose():Void;
	/**
	 * Notifies all data objects currently registered advisory sinks that data in the
	 * object has changed.
	 * @param format A , passed by reference, which describes the format, target
	 * device, rendering, and storage information of the calling data object.
	 * @param stgmedium A , passed by reference, which defines the storage medium
	 * (global memory, disk file, storage object, stream object, Graphics Device
	 * Interface (GDI) object, or undefined) and ownership of that medium for the
	 * calling data object.
	 */
	function OnDataChange(format:cs.Ref<cs.system.runtime.interopservices.comtypes.FORMATETC>, stgmedium:cs.Ref<cs.system.runtime.interopservices.comtypes.STGMEDIUM>):Void;
	/**
	 * Notifies all registered advisory sinks that the object has been renamed. This
	 * method is called by a server.
	 * @param moniker A pointer to the  interface on the new full moniker of the
	 * object.
	 */
	function OnRename(moniker:cs.system.runtime.interopservices.comtypes.IMoniker):Void;
	/** Notifies all registered advisory sinks that the object has been saved. This method is called by a server. */
	function OnSave():Void;
	/**
	 * Notifies an object's registered advisory sinks that its view has changed. This
	 * method is called by a server.
	 * @param aspect The aspect, or view, of the object. Contains a value taken from
	 * the  enumeration.
	 * @param index The portion of the view that has changed. Currently, only -1 is
	 * valid.
	 */
	function OnViewChange(aspect:Int, index:Int):Void;
}
