package cs.system.data;

/** Specifies SQL Server-specific data type of a field, property, for use in a . */
@:native("System.Data.SqlDbType")
extern enum abstract SqlDbType(Int) {
	var BigInt = 0;
	var Binary = 1;
	var Bit = 2;
	var Char = 3;
	var Date = 31;
	var DateTime = 4;
	var DateTime2 = 33;
	var DateTimeOffset = 34;
	var Decimal = 5;
	var Float = 6;
	var Image = 7;
	var Int = 8;
	var Money = 9;
	var NChar = 10;
	var NText = 11;
	var NVarChar = 12;
	var Real = 13;
	var SmallDateTime = 15;
	var SmallInt = 16;
	var SmallMoney = 17;
	var Structured = 30;
	var Text = 18;
	var Time = 32;
	var Timestamp = 19;
	var TinyInt = 20;
	var Udt = 29;
	var UniqueIdentifier = 14;
	var VarBinary = 21;
	var VarChar = 22;
	var Variant = 23;
	var Xml = 25;
}
