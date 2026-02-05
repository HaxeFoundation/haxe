package cs.system.runtime.interopservices.comtypes;

/** Describes the exceptions that occur during . */
@:native("System.Runtime.InteropServices.ComTypes.EXCEPINFO")
extern class EXCEPINFO extends cs.system.ValueType {
	/** Describes the error intended for the customer. */
	var bstrDescription:String;
	/** Contains the fully-qualified drive, path, and file name of a Help file that contains more information about the error. */
	var bstrHelpFile:String;
	/** Indicates the name of the source of the exception. Typically, this is an application name. */
	var bstrSource:String;
	/** Indicates the Help context ID of the topic within the Help file. */
	var dwHelpContext:Int;
	/** Represents a pointer to a function that takes an  structure as an argument and returns an HRESULT value. If deferred fill-in is not desired, this field is set to . */
	var pfnDeferredFillIn:cs.system.IntPtr;
	/** This field is reserved; it must be set to . */
	var pvReserved:cs.system.IntPtr;
	/** A return value describing the error. */
	var scode:Int;
	/** Represents an error code identifying the error. */
	var wCode:cs.Int16;
	/** This field is reserved; it must be set to 0. */
	var wReserved:cs.Int16;
}
