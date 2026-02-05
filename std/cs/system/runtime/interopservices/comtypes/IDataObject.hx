package cs.system.runtime.interopservices.comtypes;

/** Provides the managed definition of the  interface. */
@:native("System.Runtime.InteropServices.ComTypes.IDataObject")
extern interface IDataObject {
	/**
	 * Creates a connection between a data object and an advisory sink. This method is
	 * called by an object that supports an advisory sink and enables the advisory sink
	 * to be notified of changes in the object's data.
	 * @param pFormatetc A  structure, passed by reference, that defines the format,
	 * target device, aspect, and medium that will be used for future notifications.
	 * @param advf One of the  values that specifies a group of flags for controlling
	 * the advisory connection.
	 * @param adviseSink A pointer to the  interface on the advisory sink that will
	 * receive the change notification.
	 * @param connection When this method returns, contains a pointer to a DWORD token
	 * that identifies this connection. You can use this token later to delete the
	 * advisory connection by passing it to . If this value is zero, the connection was
	 * not established. This parameter is passed uninitialized.
	 * @return This method supports the standard return values E_INVALIDARG,
	 * E_UNEXPECTED, and E_OUTOFMEMORY, as well as the following: Value Description
	 * S_OK The advisory connection was created. E_NOTIMPL This method is not
	 * implemented on the data object. DV_E_LINDEX There is an invalid value for ;
	 * currently, only -1 is supported. DV_E_FORMATETC There is an invalid value for
	 * the  parameter. OLE_E_ADVISENOTSUPPORTED The data object does not support change
	 * notification.
	 */
	function DAdvise(pFormatetc:cs.Ref<cs.system.runtime.interopservices.comtypes.FORMATETC>, advf:cs.system.runtime.interopservices.comtypes.ADVF, adviseSink:cs.system.runtime.interopservices.comtypes.IAdviseSink, connection:cs.Ref<Int>):Int;
	/**
	 * Destroys a notification connection that had been previously established.
	 * @param connection A DWORD token that specifies the connection to remove. Use the
	 * value returned by  when the connection was originally established.
	 */
	function DUnadvise(connection:Int):Void;
	/**
	 * Creates an object that can be used to enumerate the current advisory
	 * connections.
	 * @param enumAdvise When this method returns, contains an  that receives the
	 * interface pointer to the new enumerator object. If the implementation sets  to ,
	 * there are no connections to advisory sinks at this time. This parameter is
	 * passed uninitialized.
	 * @return This method supports the standard return value E_OUTOFMEMORY, as well as
	 * the following: Value Description S_OK The enumerator object is successfully
	 * instantiated or there are no connections. OLE_E_ADVISENOTSUPPORTED This object
	 * does not support advisory notifications.
	 */
	function EnumDAdvise(enumAdvise:cs.Ref<cs.system.runtime.interopservices.comtypes.IEnumSTATDATA>):Int;
	/**
	 * Creates an object for enumerating the  structures for a data object. These
	 * structures are used in calls to  or .
	 * @param direction One of the  values that specifies the direction of the data.
	 * @return This method supports the standard return values E_INVALIDARG and
	 * E_OUTOFMEMORY, as well as the following: Value Description S_OK The enumerator
	 * object was successfully created. E_NOTIMPL The direction specified by the 
	 * parameter is not supported. OLE_S_USEREG Requests that OLE enumerate the formats
	 * from the registry.
	 */
	function EnumFormatEtc(direction:cs.system.runtime.interopservices.comtypes.DATADIR):cs.system.runtime.interopservices.comtypes.IEnumFORMATETC;
	/**
	 * Provides a standard  structure that is logically equivalent to a more complex
	 * structure. Use this method to determine whether two different  structures would
	 * return the same data, removing the need for duplicate rendering.
	 * @param formatIn A pointer to a  structure, passed by reference, that defines the
	 * format, medium, and target device that the caller would like to use to retrieve
	 * data in a subsequent call such as . The  member is not significant in this case
	 * and should be ignored.
	 * @param formatOut When this method returns, contains a pointer to a  structure
	 * that contains the most general information possible for a specific rendering,
	 * making it canonically equivalent to formatetcIn. The caller must allocate this
	 * structure and the  method must fill in the data. To retrieve data in a
	 * subsequent call such as , the caller uses the supplied value of , unless the
	 * value supplied is . This value is  if the method returns . The  member is not
	 * significant in this case and should be ignored. This parameter is passed
	 * uninitialized.
	 * @return This method supports the standard return values E_INVALIDARG,
	 * E_UNEXPECTED, and E_OUTOFMEMORY, as well as the following: Value Description
	 * S_OK The returned  structure is different from the one that was passed.
	 * DATA_S_SAMEFORMATETC The  structures are the same and  is returned in the 
	 * parameter. DV_E_LINDEX There is an invalid value for ; currently, only -1 is
	 * supported. DV_E_FORMATETC There is an invalid value for the  parameter.
	 * OLE_E_NOTRUNNING The application is not running.
	 */
	function GetCanonicalFormatEtc(formatIn:cs.Ref<cs.system.runtime.interopservices.comtypes.FORMATETC>, formatOut:cs.Ref<cs.system.runtime.interopservices.comtypes.FORMATETC>):Int;
	/**
	 * Obtains data from a source data object. The  method, which is called by a data
	 * consumer, renders the data described in the specified  structure and transfers
	 * it through the specified  structure. The caller then assumes responsibility for
	 * releasing the  structure.
	 * @param format A pointer to a  structure, passed by reference, that defines the
	 * format, medium, and target device to use when passing the data. It is possible
	 * to specify more than one medium by using the Boolean OR operator, allowing the
	 * method to choose the best medium among those specified.
	 * @param medium When this method returns, contains a pointer to the  structure
	 * that indicates the storage medium containing the returned data through its 
	 * member, and the responsibility for releasing the medium through the value of its
	 * member. If  is , the receiver of the medium is responsible for releasing it;
	 * otherwise,  points to the  interface on the appropriate object so its  method
	 * can be called. The medium must be allocated and filled in by . This parameter is
	 * passed uninitialized.
	 */
	function GetData(format:cs.Ref<cs.system.runtime.interopservices.comtypes.FORMATETC>, medium:cs.Ref<cs.system.runtime.interopservices.comtypes.STGMEDIUM>):Void;
	/**
	 * Obtains data from a source data object. This method, which is called by a data
	 * consumer, differs from the  method in that the caller must allocate and free the
	 * specified storage medium.
	 * @param format A pointer to a  structure, passed by reference, that defines the
	 * format, medium, and target device to use when passing the data. Only one medium
	 * can be specified in , and only the following  values are valid: , , , or .
	 * @param medium A , passed by reference, that defines the storage medium
	 * containing the data being transferred. The medium must be allocated by the
	 * caller and filled in by . The caller must also free the medium. The
	 * implementation of this method must always supply a value of  for the  member of
	 * the  structure that this parameter points to.
	 */
	function GetDataHere(format:cs.Ref<cs.system.runtime.interopservices.comtypes.FORMATETC>, medium:cs.Ref<cs.system.runtime.interopservices.comtypes.STGMEDIUM>):Void;
	/**
	 * Determines whether the data object is capable of rendering the data described in
	 * the  structure. Objects attempting a paste or drop operation can call this
	 * method before calling  to get an indication of whether the operation may be
	 * successful.
	 * @param format A pointer to a  structure, passed by reference, that defines the
	 * format, medium, and target device to use for the query.
	 * @return This method supports the standard return values E_INVALIDARG,
	 * E_UNEXPECTED, and E_OUTOFMEMORY, as well as the following: Value Description
	 * S_OK A subsequent call to  would probably be successful. DV_E_LINDEX An invalid
	 * value for ; currently, only -1 is supported. DV_E_FORMATETC An invalid value for
	 * the  parameter. DV_E_TYMED An invalid  value. DV_E_DVASPECT An invalid  value.
	 * OLE_E_NOTRUNNING The application is not running.
	 */
	function QueryGetData(format:cs.Ref<cs.system.runtime.interopservices.comtypes.FORMATETC>):Int;
	/**
	 * Transfers data to the object that implements this method. This method is called
	 * by an object that contains a data source.
	 * @param formatIn A  structure, passed by reference, that defines the format used
	 * by the data object when interpreting the data contained in the storage medium.
	 * @param medium A  structure, passed by reference, that defines the storage medium
	 * in which the data is being passed.
	 * @param release to specify that the data object called, which implements , owns
	 * the storage medium after the call returns. This means that the data object must
	 * free the medium after it has been used by calling the  function.  to specify
	 * that the caller retains ownership of the storage medium, and the data object
	 * called uses the storage medium for the duration of the call only.
	 */
	function SetData(formatIn:cs.Ref<cs.system.runtime.interopservices.comtypes.FORMATETC>, medium:cs.Ref<cs.system.runtime.interopservices.comtypes.STGMEDIUM>, release:Bool):Void;
}
