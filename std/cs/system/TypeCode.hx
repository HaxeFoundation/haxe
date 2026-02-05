package cs.system;

/** Specifies the type of an object. */
@:native("System.TypeCode")
extern enum abstract TypeCode(Int) {
	var Boolean = 3;
	var Byte = 6;
	var Char = 4;
	var DateTime = 16;
	var DBNull = 2;
	var Decimal = 15;
	var Double = 14;
	var Empty = 0;
	var Int16 = 7;
	var Int32 = 9;
	var Int64 = 11;
	var Object = 1;
	var SByte = 5;
	var Single = 13;
	var String = 18;
	var UInt16 = 8;
	var UInt32 = 10;
	var UInt64 = 12;
}
