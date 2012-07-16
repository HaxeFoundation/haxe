package cs.system;

@:native("System.IConvertible") extern interface IConvertible
{
	function ToType(conversionType:cs.system.Type, provider:IFormatProvider):Dynamic;
}