package cs.system.data;

/** Specifies the data type of a field, a property, or a  object of a .NET Framework data provider. */
@:native("System.Data.DbType")
extern enum abstract DbType(Int) {
	var AnsiString = 0;
	var AnsiStringFixedLength = 22;
	var Binary = 1;
	var Boolean = 3;
	var Byte = 2;
	var Currency = 4;
	var Date = 5;
	var DateTime = 6;
	var DateTime2 = 26;
	var DateTimeOffset = 27;
	var Decimal = 7;
	var Double = 8;
	var Guid = 9;
	var Int16 = 10;
	var Int32 = 11;
	var Int64 = 12;
	var Object = 13;
	var SByte = 14;
	var Single = 15;
	var String = 16;
	var StringFixedLength = 23;
	var Time = 17;
	var UInt16 = 18;
	var UInt32 = 19;
	var UInt64 = 20;
	var VarNumeric = 21;
	var Xml = 25;
}
