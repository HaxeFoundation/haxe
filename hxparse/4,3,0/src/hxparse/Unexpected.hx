package hxparse;

/**
	Unexpected is thrown by `Parser.serror`, which is invoked when an inner
	token matching fails.

	Unlike `NoMatch`, this exception denotes that the stream is in an
	irrecoverable state because tokens have been consumed.
**/
class Unexpected<Token> extends ParserError {

	/**
		The token which was found.
	**/
	public var token:Token;

	/**
		Creates a new instance of Unexpected.
	**/
	public function new(token:Token, pos) {
		super(pos);
		this.token = token;
	}

	/**
		Returns a readable representation of `this` exception.
	**/
	override public function toString() {
		return 'Unexpected $token';
	}
}