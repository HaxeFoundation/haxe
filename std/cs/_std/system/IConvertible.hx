package system;

@:native("System.IConvertible") extern interface IConvertible
{
	function ToType(conversionType:system.Type, provider:IFormatProvider):Dynamic;
}