package cs.system.data;

/** Specifies the data type of a field, a property, or a  object of a .NET Framework data provider. */
@:native("System.Data.DbType")
extern enum DbType {
	AnsiString;
	AnsiStringFixedLength;
	Binary;
	Boolean;
	Byte;
	Currency;
	Date;
	DateTime;
	DateTime2;
	DateTimeOffset;
	Decimal;
	Double;
	Guid;
	Int16;
	Int32;
	Int64;
	Object;
	SByte;
	Single;
	String;
	StringFixedLength;
	Time;
	UInt16;
	UInt32;
	UInt64;
	VarNumeric;
	Xml;
}
