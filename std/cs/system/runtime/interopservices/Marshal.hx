package cs.system.runtime.interopservices;

/** Provides a collection of methods for allocating unmanaged memory, copying unmanaged memory blocks, and converting managed to unmanaged types, as well as other miscellaneous methods used when interacting with unmanaged code. */
@:native("System.Runtime.InteropServices.Marshal")
extern class Marshal {
	/** Represents the default character size on the system; the default is 2 for Unicode systems and 1 for ANSI systems. This field is read-only. */
	static var SystemDefaultCharSize(default, never):Int;
	/** Represents the maximum size of a double byte character set (DBCS) size, in bytes, for the current operating system. This field is read-only. */
	static var SystemMaxDBCSCharSize(default, never):Int;
	/**
	 * Increments the reference count on the specified interface.
	 * @param pUnk The interface reference count to increment.
	 * @return The new value of the reference count on the  parameter.
	 */
	static function AddRef(pUnk:cs.system.IntPtr):Int;
	/**
	 * Allocates a block of memory of specified size from the COM task memory
	 * allocator.
	 * @param cb The size of the block of memory to be allocated.
	 * @return An integer representing the address of the block of memory allocated.
	 * This memory must be released with .
	 */
	static function AllocCoTaskMem(cb:Int):cs.system.IntPtr;
	@:overload(function(cb:Int):cs.system.IntPtr {})
	/**
	 * Allocates memory from the unmanaged memory of the process by using the specified
	 * number of bytes.
	 * @param cb The required number of bytes in memory.
	 * @return A pointer to the newly allocated memory. This memory must be released
	 * using the  method.
	 */
	static function AllocHGlobal(cb:cs.system.IntPtr):cs.system.IntPtr;
	/**
	 * Indicates whether runtime callable wrappers (RCWs) from any context are
	 * available for cleanup.
	 * @return if there are any RCWs available for cleanup; otherwise, .
	 */
	static function AreComObjectsAvailableForCleanup():Bool;
	/**
	 * Gets an interface pointer identified by the specified moniker.
	 * @param monikerName The moniker corresponding to the desired interface pointer.
	 * @return An object containing a reference to the interface pointer identified by
	 * the  parameter. A moniker is a name, and in this case, the moniker is defined by
	 * an interface.
	 */
	static function BindToMoniker(monikerName:String):Dynamic;
	/**
	 * Changes the strength of an object's COM Callable Wrapper (CCW) handle.
	 * @param otp The object whose CCW holds a reference counted handle. The handle is
	 * strong if the reference count on the CCW is greater than zero; otherwise, it is
	 * weak.
	 * @param fIsWeak to change the strength of the handle on the  parameter to weak,
	 * regardless of its reference count;  to reset the handle strength on  to be
	 * reference counted.
	 */
	static function ChangeWrapperHandleStrength(otp:Dynamic, fIsWeak:Bool):Void;
	/** Notifies the runtime to clean up all Runtime Callable Wrappers (RCWs) allocated in the current context. */
	static function CleanupUnusedObjectsInCurrentContext():Void;
	@:overload(function(source:cs.NativeArray<cs.UInt8>, startIndex:Int, destination:cs.system.IntPtr, length:Int):Void {})
	@:overload(function(source:cs.NativeArray<cs.Char16>, startIndex:Int, destination:cs.system.IntPtr, length:Int):Void {})
	@:overload(function(source:cs.NativeArray<Float>, startIndex:Int, destination:cs.system.IntPtr, length:Int):Void {})
	@:overload(function(source:cs.NativeArray<cs.Int16>, startIndex:Int, destination:cs.system.IntPtr, length:Int):Void {})
	@:overload(function(source:cs.NativeArray<Int>, startIndex:Int, destination:cs.system.IntPtr, length:Int):Void {})
	@:overload(function(source:cs.NativeArray<haxe.Int64>, startIndex:Int, destination:cs.system.IntPtr, length:Int):Void {})
	@:overload(function(source:cs.system.IntPtr, destination:cs.NativeArray<cs.UInt8>, startIndex:Int, length:Int):Void {})
	@:overload(function(source:cs.system.IntPtr, destination:cs.NativeArray<cs.Char16>, startIndex:Int, length:Int):Void {})
	@:overload(function(source:cs.system.IntPtr, destination:cs.NativeArray<Float>, startIndex:Int, length:Int):Void {})
	@:overload(function(source:cs.system.IntPtr, destination:cs.NativeArray<cs.Int16>, startIndex:Int, length:Int):Void {})
	@:overload(function(source:cs.system.IntPtr, destination:cs.NativeArray<Int>, startIndex:Int, length:Int):Void {})
	@:overload(function(source:cs.system.IntPtr, destination:cs.NativeArray<haxe.Int64>, startIndex:Int, length:Int):Void {})
	@:overload(function(source:cs.system.IntPtr, destination:cs.NativeArray<cs.system.IntPtr>, startIndex:Int, length:Int):Void {})
	@:overload(function(source:cs.system.IntPtr, destination:cs.NativeArray<Single>, startIndex:Int, length:Int):Void {})
	@:overload(function(source:cs.NativeArray<cs.system.IntPtr>, startIndex:Int, destination:cs.system.IntPtr, length:Int):Void {})
	/**
	 * Copies data from a one-dimensional, managed 8-bit unsigned integer array to an
	 * unmanaged memory pointer.
	 * @param source The one-dimensional array to copy from.
	 * @param startIndex The zero-based index in the source array where copying should
	 * start.
	 * @param destination The memory pointer to copy to.
	 * @param length The number of array elements to copy.
	 */
	static function Copy(source:cs.NativeArray<Single>, startIndex:Int, destination:cs.system.IntPtr, length:Int):Void;
	@:overload(function(pOuter:cs.system.IntPtr, o:Dynamic):cs.system.IntPtr {})
	/**
	 * Aggregates a managed object with the specified COM object.
	 * @param pOuter The outer  pointer.
	 * @param o An object to aggregate.
	 * @return The inner  pointer of the managed object.
	 */
	static function CreateAggregatedObject<T>(pOuter:cs.system.IntPtr, o:T):cs.system.IntPtr;
	@:overload(function<T, TWrapper>(o:T):TWrapper {})
	/**
	 * Wraps the specified COM object in an object of the specified type.
	 * @param o The object to be wrapped.
	 * @param t The type of wrapper to create.
	 * @return The newly wrapped object that is an instance of the desired type.
	 */
	static function CreateWrapperOfType(o:Dynamic, t:cs.system.Type):Dynamic;
	@:overload(function<T>(ptr:cs.system.IntPtr):Void {})
	/**
	 * Frees all substructures that the specified unmanaged memory block points to.
	 * @param ptr A pointer to an unmanaged block of memory.
	 * @param structuretype Type of a formatted class. This provides the layout
	 * information necessary to delete the buffer in the  parameter.
	 */
	static function DestroyStructure(ptr:cs.system.IntPtr, structuretype:cs.system.Type):Void;
	/**
	 * Releases all references to a Runtime Callable Wrapper (RCW) by setting its
	 * reference count to 0.
	 * @param o The RCW to be released.
	 * @return The new value of the reference count of the RCW associated with the 
	 * parameter, which is 0 (zero) if the release is successful.
	 */
	static function FinalReleaseComObject(o:Dynamic):Int;
	/**
	 * Frees a  using the COM SysFreeString function.
	 * @param ptr The address of the BSTR to be freed.
	 */
	static function FreeBSTR(ptr:cs.system.IntPtr):Void;
	/**
	 * Frees a block of memory allocated by the unmanaged COM task memory allocator.
	 * @param ptr The address of the memory to be freed.
	 */
	static function FreeCoTaskMem(ptr:cs.system.IntPtr):Void;
	/**
	 * Frees memory previously allocated from the unmanaged memory of the process.
	 * @param hglobal The handle returned by the original matching call to .
	 */
	static function FreeHGlobal(hglobal:cs.system.IntPtr):Void;
	/**
	 * Returns the globally unique identifier (GUID) for the specified type, or
	 * generates a GUID using the algorithm used by the Type Library Exporter
	 * (Tlbexp.exe).
	 * @param type The type to generate a GUID for.
	 * @return An identifier for the specified type.
	 */
	static function GenerateGuidForType(type:cs.system.Type):cs.system.Guid;
	/**
	 * Returns a programmatic identifier (ProgID) for the specified type.
	 * @param type The type to get a ProgID for.
	 * @return The ProgID of the specified type.
	 */
	static function GenerateProgIdForType(type:cs.system.Type):String;
	@:overload(function<T, TInterface>(o:T):cs.system.IntPtr {})
	@:overload(function(o:Dynamic, T:cs.system.Type):cs.system.IntPtr {})
	/**
	 * Returns a pointer to an IUnknown interface that represents the specified
	 * interface on the specified object. Custom query interface access is enabled by
	 * default.
	 * @param o The object that provides the interface.
	 * @param T The type of interface that is requested.
	 * @return The interface pointer that represents the specified interface for the
	 * object.
	 */
	static function GetComInterfaceForObject(o:Dynamic, T:cs.system.Type, mode:cs.system.runtime.interopservices.CustomQueryInterfaceMode):cs.system.IntPtr;
	/**
	 * Retrieves data that is referenced by the specified key from the specified COM
	 * object.
	 * @param obj The COM object that contains the data that you want.
	 * @param key The key in the internal hash table of  to retrieve the data from.
	 * @return The data represented by the  parameter in the internal hash table of the
	 * parameter.
	 */
	static function GetComObjectData(obj:Dynamic, key:Dynamic):Dynamic;
	@:overload(function<TDelegate>(ptr:cs.system.IntPtr):TDelegate {})
	/**
	 * Converts an unmanaged function pointer to a delegate.
	 * @param ptr The unmanaged function pointer to be converted.
	 * @param t The type of the delegate to be returned.
	 * @return A delegate instance that can be cast to the appropriate delegate type.
	 */
	static function GetDelegateForFunctionPointer(ptr:cs.system.IntPtr, t:cs.system.Type):cs.system.Delegate;
	/**
	 * Retrieves a code that identifies the type of the exception that occurred.
	 * @return The type of the exception.
	 */
	static function GetExceptionCode():Int;
	@:overload(function(errorCode:Int):cs.system.Exception {})
	/**
	 * Converts the specified HRESULT error code to a corresponding  object.
	 * @param errorCode The HRESULT to be converted.
	 * @return An object that represents the converted HRESULT, or  if the HRESULT
	 * value doesn't represent an error code (for example,  or ).
	 */
	static function GetExceptionForHR(errorCode:Int, errorInfo:cs.system.IntPtr):cs.system.Exception;
	@:overload(function(d:cs.system.Delegate):cs.system.IntPtr {})
	/**
	 * Converts a delegate into a function pointer that is callable from unmanaged
	 * code.
	 * @param d The delegate to be passed to unmanaged code.
	 * @return A value that can be passed to unmanaged code, which, in turn, can use it
	 * to call the underlying managed delegate.
	 */
	static function GetFunctionPointerForDelegate<TDelegate>(d:TDelegate):cs.system.IntPtr;
	/**
	 * Returns the instance handle (HINSTANCE) for the specified module.
	 * @param m The module whose HINSTANCE is desired.
	 * @return The HINSTANCE for ; or -1 if the module does not have an HINSTANCE.
	 */
	static function GetHINSTANCE(m:cs.system.reflection.Module):cs.system.IntPtr;
	/**
	 * Converts the specified exception to an HRESULT.
	 * @param e The exception to convert to an HRESULT.
	 * @return The HRESULT mapped to the supplied exception.
	 */
	static function GetHRForException(e:cs.system.Exception):Int;
	/**
	 * Returns the HRESULT corresponding to the last error incurred by Win32 code
	 * executed using .
	 * @return The HRESULT corresponding to the last Win32 error code.
	 */
	static function GetHRForLastWin32Error():Int;
	/**
	 * Returns an IDispatch interface from a managed object.
	 * @param o The object whose  interface is requested.
	 * @return The  pointer for the  parameter.
	 */
	static function GetIDispatchForObject(o:Dynamic):cs.system.IntPtr;
	/**
	 * Returns an IUnknown interface from a managed object.
	 * @param o The object whose  interface is requested.
	 * @return The  pointer for the  parameter.
	 */
	static function GetIUnknownForObject(o:Dynamic):cs.system.IntPtr;
	/**
	 * Returns the error code returned by the last unmanaged function that was called
	 * using platform invoke that has the  flag set.
	 * @return The last error code set by a call to the Win32 SetLastError function.
	 */
	static function GetLastWin32Error():Int;
	@:overload(function(obj:Dynamic, pDstNativeVariant:cs.system.IntPtr):Void {})
	/**
	 * Converts an object to a COM VARIANT.
	 * @param obj The object for which to get a COM VARIANT.
	 * @param pDstNativeVariant A pointer to receive the VARIANT that corresponds to
	 * the  parameter.
	 */
	static function GetNativeVariantForObject<T>(obj:T, pDstNativeVariant:cs.system.IntPtr):Void;
	/**
	 * Returns an instance of a type that represents a COM object by a pointer to its
	 * IUnknown interface.
	 * @param pUnk A pointer to the  interface.
	 * @return An object that represents the specified unmanaged COM object.
	 */
	static function GetObjectForIUnknown(pUnk:cs.system.IntPtr):Dynamic;
	@:overload(function(pSrcNativeVariant:cs.system.IntPtr):Dynamic {})
	/**
	 * Converts a COM VARIANT to an object.
	 * @param pSrcNativeVariant A pointer to a COM VARIANT.
	 * @return An object that corresponds to the  parameter.
	 */
	static function GetObjectForNativeVariant<T>(pSrcNativeVariant:cs.system.IntPtr):T;
	@:overload(function(aSrcNativeVariant:cs.system.IntPtr, cVars:Int):cs.NativeArray<Dynamic> {})
	/**
	 * Converts an array of COM VARIANTs to an array of objects.
	 * @param aSrcNativeVariant A pointer to the first element of an array of COM
	 * VARIANTs.
	 * @param cVars The count of COM VARIANTs in .
	 * @return An object array that corresponds to .
	 */
	static function GetObjectsForNativeVariants<T>(aSrcNativeVariant:cs.system.IntPtr, cVars:Int):cs.NativeArray<T>;
	/**
	 * Gets the first slot in the virtual function table (v-table or VTBL) that
	 * contains user-defined methods.
	 * @param t A type that represents an interface or a class.
	 * @return The first VTBL slot that contains user-defined methods. The first slot
	 * is 3 if the interface is based on IUnknown, and 7 if the interface is based on
	 * IDispatch.
	 */
	static function GetStartComSlot(t:cs.system.Type):Int;
	/**
	 * Returns a managed object of a specified type that represents a COM object.
	 * @param pUnk A pointer to the  interface of the unmanaged object.
	 * @param t The type of the requested managed class.
	 * @return An instance of the class corresponding to the  object that represents
	 * the requested unmanaged COM object.
	 */
	static function GetTypedObjectForIUnknown(pUnk:cs.system.IntPtr, t:cs.system.Type):Dynamic;
	/**
	 * Returns the type associated with the specified class identifier (CLSID).
	 * @param clsid The CLSID of the type to return.
	 * @return regardless of whether the CLSID is valid.
	 */
	static function GetTypeFromCLSID(clsid:cs.system.Guid):cs.system.Type;
	/**
	 * Retrieves the name of the type represented by an ITypeInfo object.
	 * @param typeInfo An object that represents an  pointer.
	 * @return The name of the type that the  parameter points to.
	 */
	static function GetTypeInfoName(typeInfo:cs.system.runtime.interopservices.comtypes.ITypeInfo):String;
	/**
	 * Creates a unique Runtime Callable Wrapper (RCW) object for a given IUnknown
	 * interface.
	 * @param unknown A managed pointer to an  interface.
	 * @return A unique RCW for the specified  interface.
	 */
	static function GetUniqueObjectForIUnknown(unknown:cs.system.IntPtr):Dynamic;
	/**
	 * Indicates whether a specified object represents a COM object.
	 * @param o The object to check.
	 * @return if the  parameter is a COM type; otherwise, .
	 */
	static function IsComObject(o:Dynamic):Bool;
	@:overload(function<T>(fieldName:String):cs.system.IntPtr {})
	/**
	 * Returns the field offset of the unmanaged form of the managed class.
	 * @param t A value type or formatted reference type that specifies the managed
	 * class. You must apply the  to the class.
	 * @param fieldName The field within the  parameter.
	 * @return The offset, in bytes, for the  parameter within the specified class that
	 * is declared by platform invoke.
	 */
	static function OffsetOf(t:cs.system.Type, fieldName:String):cs.system.IntPtr;
	/**
	 * Executes one-time method setup tasks without calling the method.
	 * @param m The method to be checked.
	 */
	static function Prelink(m:cs.system.reflection.MethodInfo):Void;
	/**
	 * Performs a pre-link check for all methods on a class.
	 * @param c The class whose methods are to be checked.
	 */
	static function PrelinkAll(c:cs.system.Type):Void;
	@:overload(function(ptr:cs.system.IntPtr):String {})
	/**
	 * Copies all characters up to the first null character from an unmanaged ANSI
	 * string to a managed , and widens each ANSI character to Unicode.
	 * @param ptr The address of the first character of the unmanaged string.
	 * @return A managed string that holds a copy of the unmanaged ANSI string. If  is
	 * , the method returns a null string.
	 */
	static function PtrToStringAnsi(ptr:cs.system.IntPtr, len:Int):String;
	@:overload(function(ptr:cs.system.IntPtr):String {})
	/**
	 * Allocates a managed  and copies all characters up to the first null character
	 * from a string stored in unmanaged memory into it.
	 * @param ptr For Unicode platforms, the address of the first Unicode character.
	 * -or- For ANSI platforms, the address of the first ANSI character.
	 * @return A managed string that holds a copy of the unmanaged string if the value
	 * of the  parameter is not ; otherwise, this method returns .
	 */
	static function PtrToStringAuto(ptr:cs.system.IntPtr, len:Int):String;
	/**
	 * Allocates a managed  and copies a binary string (BSTR) stored in unmanaged
	 * memory into it.
	 * @param ptr The address of the first character of the unmanaged string.
	 * @return A managed string that holds a copy of the unmanaged string.
	 */
	static function PtrToStringBSTR(ptr:cs.system.IntPtr):String;
	@:overload(function(ptr:cs.system.IntPtr):String {})
	/**
	 * Allocates a managed  and copies all characters up to the first null character
	 * from an unmanaged Unicode string into it.
	 * @param ptr The address of the first character of the unmanaged string.
	 * @return A managed string that holds a copy of the unmanaged string if the value
	 * of the  parameter is not ; otherwise, this method returns .
	 */
	static function PtrToStringUni(ptr:cs.system.IntPtr, len:Int):String;
	@:overload(function(ptr:cs.system.IntPtr):String {})
	/** @param ptr  */
	static function PtrToStringUTF8(ptr:cs.system.IntPtr, byteLen:Int):String;
	@:overload(function<T>(ptr:cs.system.IntPtr):T {})
	@:overload(function(ptr:cs.system.IntPtr, structure:Dynamic):Void {})
	@:overload(function(ptr:cs.system.IntPtr, structureType:cs.system.Type):Dynamic {})
	/**
	 * Marshals data from an unmanaged block of memory to a managed object.
	 * @param ptr A pointer to an unmanaged block of memory.
	 * @param structure The object to which the data is to be copied. This must be an
	 * instance of a formatted class.
	 */
	static function PtrToStructure<T>(ptr:cs.system.IntPtr, structure:T):Void;
	/**
	 * Requests a pointer to a specified interface from a COM object.
	 * @param pUnk The interface to be queried.
	 * @param iid The interface identifier (IID) of the requested interface.
	 * @param ppv When this method returns, contains a reference to the returned
	 * interface.
	 * @return An HRESULT that indicates the success or failure of the call.
	 */
	static function QueryInterface(pUnk:cs.system.IntPtr, iid:cs.Ref<cs.system.Guid>, ppv:cs.Ref<cs.system.IntPtr>):Int;
	@:overload(function(ptr:cs.system.IntPtr):cs.UInt8 {})
	@:overload(function(ptr:cs.system.IntPtr, ofs:Int):cs.UInt8 {})
	/**
	 * Reads a single byte from unmanaged memory.
	 * @param ptr The address in unmanaged memory from which to read.
	 * @return The byte read from unmanaged memory.
	 */
	static function ReadByte(ptr:Dynamic, ofs:Int):cs.UInt8;
	@:overload(function(ptr:cs.system.IntPtr):cs.Int16 {})
	@:overload(function(ptr:cs.system.IntPtr, ofs:Int):cs.Int16 {})
	/**
	 * Reads a 16-bit signed integer from unmanaged memory.
	 * @param ptr The address in unmanaged memory from which to read.
	 * @return The 16-bit signed integer read from unmanaged memory.
	 */
	static function ReadInt16(ptr:Dynamic, ofs:Int):cs.Int16;
	@:overload(function(ptr:cs.system.IntPtr):Int {})
	@:overload(function(ptr:cs.system.IntPtr, ofs:Int):Int {})
	/**
	 * Reads a 32-bit signed integer from unmanaged memory.
	 * @param ptr The address in unmanaged memory from which to read.
	 * @return The 32-bit signed integer read from unmanaged memory.
	 */
	static function ReadInt32(ptr:Dynamic, ofs:Int):Int;
	@:overload(function(ptr:cs.system.IntPtr):haxe.Int64 {})
	@:overload(function(ptr:cs.system.IntPtr, ofs:Int):haxe.Int64 {})
	/**
	 * Reads a 64-bit signed integer from unmanaged memory.
	 * @param ptr The address in unmanaged memory from which to read.
	 * @return The 64-bit signed integer read from unmanaged memory.
	 */
	static function ReadInt64(ptr:Dynamic, ofs:Int):haxe.Int64;
	@:overload(function(ptr:cs.system.IntPtr):cs.system.IntPtr {})
	@:overload(function(ptr:cs.system.IntPtr, ofs:Int):cs.system.IntPtr {})
	/**
	 * Reads a processor native-sized integer from unmanaged memory.
	 * @param ptr The address in unmanaged memory from which to read.
	 * @return The integer read from unmanaged memory. A 32 bit integer is returned on
	 * 32 bit machines and a 64 bit integer is returned on 64 bit machines.
	 */
	static function ReadIntPtr(ptr:Dynamic, ofs:Int):cs.system.IntPtr;
	/**
	 * Resizes a block of memory previously allocated with .
	 * @param pv A pointer to memory allocated with .
	 * @param cb The new size of the allocated block.
	 * @return An integer representing the address of the reallocated block of memory.
	 * This memory must be released with .
	 */
	static function ReAllocCoTaskMem(pv:cs.system.IntPtr, cb:Int):cs.system.IntPtr;
	/**
	 * Resizes a block of memory previously allocated with .
	 * @param pv A pointer to memory allocated with .
	 * @param cb The new size of the allocated block. This is not a pointer; it is the
	 * byte count you are requesting, cast to type . If you pass a pointer, it is
	 * treated as a size.
	 * @return A pointer to the reallocated memory. This memory must be released using
	 * .
	 */
	static function ReAllocHGlobal(pv:cs.system.IntPtr, cb:cs.system.IntPtr):cs.system.IntPtr;
	/**
	 * Decrements the reference count on the specified interface.
	 * @param pUnk The interface to release.
	 * @return The new value of the reference count on the interface specified by the 
	 * parameter.
	 */
	static function Release(pUnk:cs.system.IntPtr):Int;
	/**
	 * Decrements the reference count of the Runtime Callable Wrapper (RCW) associated
	 * with the specified COM object.
	 * @param o The COM object to release.
	 * @return The new value of the reference count of the RCW associated with . This
	 * value is typically zero since the RCW keeps just one reference to the wrapped
	 * COM object regardless of the number of managed clients calling it.
	 */
	static function ReleaseComObject(o:Dynamic):Int;
	/**
	 * Allocates an unmanaged binary string (BSTR) and copies the contents of a managed
	 * object into it.
	 * @param s The managed object to copy.
	 * @return The address, in unmanaged memory, where the  parameter was copied to, or
	 * 0 if a null object was supplied.
	 */
	static function SecureStringToBSTR(s:cs.system.security.SecureString):cs.system.IntPtr;
	/**
	 * Copies the contents of a managed  object to a block of memory allocated from the
	 * unmanaged COM task allocator.
	 * @param s The managed object to copy.
	 * @return The address, in unmanaged memory, where the  parameter was copied to, or
	 * 0 if a null object was supplied.
	 */
	static function SecureStringToCoTaskMemAnsi(s:cs.system.security.SecureString):cs.system.IntPtr;
	/**
	 * Copies the contents of a managed  object to a block of memory allocated from the
	 * unmanaged COM task allocator.
	 * @param s The managed object to copy.
	 * @return The address, in unmanaged memory, where the  parameter was copied to, or
	 * 0 if a null object was supplied.
	 */
	static function SecureStringToCoTaskMemUnicode(s:cs.system.security.SecureString):cs.system.IntPtr;
	/**
	 * Copies the contents of a managed  into unmanaged memory, converting into ANSI
	 * format as it copies.
	 * @param s The managed object to copy.
	 * @return The address, in unmanaged memory, to where the  parameter was copied, or
	 * 0 if a null object was supplied.
	 */
	static function SecureStringToGlobalAllocAnsi(s:cs.system.security.SecureString):cs.system.IntPtr;
	/**
	 * Copies the contents of a managed  object into unmanaged memory.
	 * @param s The managed object to copy.
	 * @return The address, in unmanaged memory, where  was copied, or 0 if  is a 
	 * object whose length is 0.
	 */
	static function SecureStringToGlobalAllocUnicode(s:cs.system.security.SecureString):cs.system.IntPtr;
	/**
	 * Sets data referenced by the specified key in the specified COM object.
	 * @param obj The COM object in which to store the data.
	 * @param key The key in the internal hash table of the COM object in which to
	 * store the data.
	 * @param data The data to set.
	 * @return if the data was set successfully; otherwise, .
	 */
	static function SetComObjectData(obj:Dynamic, key:Dynamic, data:Dynamic):Bool;
	@:overload(function<T>():Int {})
	@:overload(function(structure:Dynamic):Int {})
	@:overload(function(t:cs.system.Type):Int {})
	/**
	 * Returns the unmanaged size of an object in bytes.
	 * @param structure The object whose size is to be returned.
	 * @return The size of the specified object in unmanaged code.
	 */
	static function SizeOf<T>(structure:T):Int;
	/**
	 * Allocates a BSTR and copies the contents of a managed  into it.
	 * @param s The managed string to be copied.
	 * @return An unmanaged pointer to the , or 0 if  is null.
	 */
	static function StringToBSTR(s:String):cs.system.IntPtr;
	/**
	 * Copies the contents of a managed  to a block of memory allocated from the
	 * unmanaged COM task allocator.
	 * @param s A managed string to be copied.
	 * @return An integer representing a pointer to the block of memory allocated for
	 * the string, or 0 if  is .
	 */
	static function StringToCoTaskMemAnsi(s:String):cs.system.IntPtr;
	/**
	 * Copies the contents of a managed  to a block of memory allocated from the
	 * unmanaged COM task allocator.
	 * @param s A managed string to be copied.
	 * @return The allocated memory block, or 0 if  is .
	 */
	static function StringToCoTaskMemAuto(s:String):cs.system.IntPtr;
	/**
	 * Copies the contents of a managed  to a block of memory allocated from the
	 * unmanaged COM task allocator.
	 * @param s A managed string to be copied.
	 * @return An integer representing a pointer to the block of memory allocated for
	 * the string, or 0 if s is .
	 */
	static function StringToCoTaskMemUni(s:String):cs.system.IntPtr;
	/** @param s  */
	static function StringToCoTaskMemUTF8(s:String):cs.system.IntPtr;
	/**
	 * Copies the contents of a managed  into unmanaged memory, converting into ANSI
	 * format as it copies.
	 * @param s A managed string to be copied.
	 * @return The address, in unmanaged memory, to where  was copied, or 0 if  is .
	 */
	static function StringToHGlobalAnsi(s:String):cs.system.IntPtr;
	/**
	 * Copies the contents of a managed  into unmanaged memory, converting into ANSI
	 * format if required.
	 * @param s A managed string to be copied.
	 * @return The address, in unmanaged memory, to where the string was copied, or 0
	 * if  is .
	 */
	static function StringToHGlobalAuto(s:String):cs.system.IntPtr;
	/**
	 * Copies the contents of a managed  into unmanaged memory.
	 * @param s A managed string to be copied.
	 * @return The address, in unmanaged memory, to where the  was copied, or 0 if  is
	 * .
	 */
	static function StringToHGlobalUni(s:String):cs.system.IntPtr;
	@:overload(function(structure:Dynamic, ptr:cs.system.IntPtr, fDeleteOld:Bool):Void {})
	/**
	 * Marshals data from a managed object to an unmanaged block of memory.
	 * @param structure A managed object that holds the data to be marshaled. This
	 * object must be a structure or an instance of a formatted class.
	 * @param ptr A pointer to an unmanaged block of memory, which must be allocated
	 * before this method is called.
	 * @param fDeleteOld to call the  method on the  parameter before this method
	 * copies the data. The block must contain valid data. Note that passing  when the
	 * memory block already contains data can lead to a memory leak.
	 */
	static function StructureToPtr<T>(structure:T, ptr:cs.system.IntPtr, fDeleteOld:Bool):Void;
	@:overload(function(errorCode:Int):Void {})
	/**
	 * Throws an exception with a specific failure HRESULT value.
	 * @param errorCode The HRESULT corresponding to the desired exception.
	 */
	static function ThrowExceptionForHR(errorCode:Int, errorInfo:cs.system.IntPtr):Void;
	@:overload(function(arr:cs.system.Array, index:Int):cs.system.IntPtr {})
	/**
	 * Gets the address of the element at the specified index inside the specified
	 * array.
	 * @param arr The array that contains the desired element.
	 * @param index The index in the  parameter of the desired element.
	 * @return The address of  inside .
	 */
	static function UnsafeAddrOfPinnedArrayElement<T>(arr:cs.NativeArray<T>, index:Int):cs.system.IntPtr;
	@:overload(function(ptr:cs.system.IntPtr, val:cs.UInt8):Void {})
	@:overload(function(ptr:cs.system.IntPtr, ofs:Int, val:cs.UInt8):Void {})
	/**
	 * Writes a single byte value to unmanaged memory.
	 * @param ptr The address in unmanaged memory to write to.
	 * @param val The value to write.
	 */
	static function WriteByte(ptr:Dynamic, ofs:Int, val:cs.UInt8):Void;
	@:overload(function(ptr:cs.system.IntPtr, val:cs.Char16):Void {})
	@:overload(function(ptr:cs.system.IntPtr, val:cs.Int16):Void {})
	@:overload(function(ptr:cs.system.IntPtr, ofs:Int, val:cs.Char16):Void {})
	@:overload(function(ptr:cs.system.IntPtr, ofs:Int, val:cs.Int16):Void {})
	@:overload(function(ptr:Dynamic, ofs:Int, val:cs.Char16):Void {})
	/**
	 * Writes a character as a 16-bit integer value to unmanaged memory.
	 * @param ptr The address in unmanaged memory to write to.
	 * @param val The value to write.
	 */
	static function WriteInt16(ptr:Dynamic, ofs:Int, val:cs.Int16):Void;
	@:overload(function(ptr:cs.system.IntPtr, val:Int):Void {})
	@:overload(function(ptr:cs.system.IntPtr, ofs:Int, val:Int):Void {})
	/**
	 * Writes a 32-bit signed integer value to unmanaged memory.
	 * @param ptr The address in unmanaged memory to write to.
	 * @param val The value to write.
	 */
	static function WriteInt32(ptr:Dynamic, ofs:Int, val:Int):Void;
	@:overload(function(ptr:cs.system.IntPtr, val:haxe.Int64):Void {})
	@:overload(function(ptr:cs.system.IntPtr, ofs:Int, val:haxe.Int64):Void {})
	/**
	 * Writes a 64-bit signed integer value to unmanaged memory at a specified offset.
	 * @param ptr The base address in unmanaged memory to write.
	 * @param ofs An additional byte offset, which is added to the  parameter before
	 * writing.
	 * @param val The value to write.
	 */
	static function WriteInt64(ptr:Dynamic, ofs:Int, val:haxe.Int64):Void;
	@:overload(function(ptr:cs.system.IntPtr, val:cs.system.IntPtr):Void {})
	@:overload(function(ptr:cs.system.IntPtr, ofs:Int, val:cs.system.IntPtr):Void {})
	/**
	 * Writes a processor native-sized integer value to unmanaged memory at a specified
	 * offset.
	 * @param ptr The base address in unmanaged memory to write to.
	 * @param ofs An additional byte offset, which is added to the  parameter before
	 * writing.
	 * @param val The value to write.
	 */
	static function WriteIntPtr(ptr:Dynamic, ofs:Int, val:cs.system.IntPtr):Void;
	/**
	 * Frees a BSTR pointer that was allocated using the  method.
	 * @param s The address of the  to free.
	 */
	static function ZeroFreeBSTR(s:cs.system.IntPtr):Void;
	/**
	 * Frees an unmanaged string pointer that was allocated using the  method.
	 * @param s The address of the unmanaged string to free.
	 */
	static function ZeroFreeCoTaskMemAnsi(s:cs.system.IntPtr):Void;
	/**
	 * Frees an unmanaged string pointer that was allocated using the  method.
	 * @param s The address of the unmanaged string to free.
	 */
	static function ZeroFreeCoTaskMemUnicode(s:cs.system.IntPtr):Void;
	/** @param s  */
	static function ZeroFreeCoTaskMemUTF8(s:cs.system.IntPtr):Void;
	/**
	 * Frees an unmanaged string pointer that was allocated using the  method.
	 * @param s The address of the unmanaged string to free.
	 */
	static function ZeroFreeGlobalAllocAnsi(s:cs.system.IntPtr):Void;
	/**
	 * Frees an unmanaged string pointer that was allocated using the  method.
	 * @param s The address of the unmanaged string to free.
	 */
	static function ZeroFreeGlobalAllocUnicode(s:cs.system.IntPtr):Void;
}
