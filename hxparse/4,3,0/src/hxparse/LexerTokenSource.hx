package hxparse;

class LexerTokenSource<Token> {
	var lexer:Lexer;
	public var ruleset:Ruleset<Token>;

	public function new(lexer, ruleset){
		this.lexer = lexer;
		this.ruleset = ruleset;
	}

	public function token():Token{
		return lexer.token(ruleset);
	}

	public function curPos():Position{
		return lexer.curPos();
	}
}