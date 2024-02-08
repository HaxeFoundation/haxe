class HaxeInvocationException {
	public var message:String;
	public var fieldName:String;
	public var arguments:Array<String>;
	public var source:String;

	public function new(message:String, fieldName:String, arguments:Array<String>, source:String) {
		this.message = message;
		this.fieldName = fieldName;
		this.arguments = arguments;
		this.source = source;
	}

	public function toString() {
		return 'HaxeInvocationException($message, $fieldName, $arguments, $source])';
	}
}

