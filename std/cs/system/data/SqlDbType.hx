package cs.system.data;

/** Specifies SQL Server-specific data type of a field, property, for use in a . */
@:native("System.Data.SqlDbType")
extern enum SqlDbType {
	BigInt;
	Binary;
	Bit;
	Char;
	Date;
	DateTime;
	DateTime2;
	DateTimeOffset;
	Decimal;
	Float;
	Image;
	Int;
	Money;
	NChar;
	NText;
	NVarChar;
	Real;
	SmallDateTime;
	SmallInt;
	SmallMoney;
	Structured;
	Text;
	Time;
	Timestamp;
	TinyInt;
	Udt;
	UniqueIdentifier;
	VarBinary;
	VarChar;
	Variant;
	Xml;
}
