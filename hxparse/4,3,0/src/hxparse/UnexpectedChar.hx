package hxparse;

/**
	UnexpectedChar is thrown by `Lexer.token` if it encounters a character for
	which no state transition is defined.
**/
class UnexpectedChar extends ParserError {

	/**
		The character which caused `this` exception.
	**/
	public var char:String;

	/**
		Creates a new instance of UnexpectedChar.
	**/
	public function new(char, pos) {
		super(pos);
		this.char = char;
	}

	/**
		Returns a readable representation of `this` exception.
	**/
	override public function toString() {
		return 'Unexpected $char';
	}
}