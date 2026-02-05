package cs.system.xml;

/** Enumerates the configurable quota values for XmlDictionaryReaders. */
@:native("System.Xml.XmlDictionaryReaderQuotaTypes")
extern enum abstract XmlDictionaryReaderQuotaTypes(Int) {
	var MaxArrayLength = 4;
	var MaxBytesPerRead = 8;
	var MaxDepth = 1;
	var MaxNameTableCharCount = 16;
	var MaxStringContentLength = 2;
	@:op(A | B) static function or(lhs:XmlDictionaryReaderQuotaTypes, rhs:XmlDictionaryReaderQuotaTypes):XmlDictionaryReaderQuotaTypes;
	@:op(A & B) static function and(lhs:XmlDictionaryReaderQuotaTypes, rhs:XmlDictionaryReaderQuotaTypes):XmlDictionaryReaderQuotaTypes;
	@:op(A ^ B) static function xor(lhs:XmlDictionaryReaderQuotaTypes, rhs:XmlDictionaryReaderQuotaTypes):XmlDictionaryReaderQuotaTypes;
	@:op(~A) static function complement(value:XmlDictionaryReaderQuotaTypes):XmlDictionaryReaderQuotaTypes;
}
