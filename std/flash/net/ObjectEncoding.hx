package flash.net;

extern class ObjectEncoding {
	static final AMF0 : UInt;
	static final AMF3 : UInt;
	static final DEFAULT : UInt;
	@:flash.property static var dynamicPropertyWriter(get,set) : IDynamicPropertyWriter;
	private static function get_dynamicPropertyWriter() : IDynamicPropertyWriter;
	private static function set_dynamicPropertyWriter(value : IDynamicPropertyWriter) : IDynamicPropertyWriter;
}
