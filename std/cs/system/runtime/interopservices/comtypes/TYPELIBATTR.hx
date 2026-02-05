package cs.system.runtime.interopservices.comtypes;

/** Identifies a particular type library and provides localization support for member names. */
@:native("System.Runtime.InteropServices.ComTypes.TYPELIBATTR")
extern class TYPELIBATTR extends cs.system.ValueType {
	/** Represents a globally unique library ID of a type library. */
	var guid:cs.system.Guid;
	/** Represents a locale ID of a type library. */
	var lcid:Int;
	/** Represents the target hardware platform of a type library. */
	var syskind:cs.system.runtime.interopservices.comtypes.SYSKIND;
	/** Represents library flags. */
	var wLibFlags:cs.system.runtime.interopservices.comtypes.LIBFLAGS;
	/** Represents the major version number of a type library. */
	var wMajorVerNum:cs.Int16;
	/** Represents the minor version number of a type library. */
	var wMinorVerNum:cs.Int16;
}
