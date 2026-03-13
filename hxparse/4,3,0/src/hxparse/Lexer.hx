package hxparse;

/**
	Lexer matches a sequence of characters against a set of rule patterns.

	An instance of Lexer is created once for each input and maintains state
	for that input. Tokens can then be obtained by calling the `token` method,
	passing an instance of `Ruleset`.

	Rule sets can be created manually, or by calling the static `buildRuleset`
	method.
**/
class Lexer {

	/**
		The `String` that was matched by the most recent invocation of the
		`token` method.
	**/
	public var current(default, null):String;

	var input:byte.ByteData;
	var source:String;
	var pos:Int;

	/**
		Creates a new Lexer for `input`.

		If `sourceName` is provided, it is used in error messages to denote
		the position of an error.

		If `input` is null, the result is unspecified.
	**/
	public function new(input:byte.ByteData, sourceName:String = "<null>") {
		current = "";
		this.input = input;
		source = sourceName;
		pos = 0;
	}

	/**
		Returns the current position of `this` Lexer.
	**/
	public inline function curPos():Position {
		return new Position(source, pos - current.length, pos);
	}

	/**
		Returns the next token according to `ruleset`.

		This method starts with `ruleset.state` and reads characters from `this`
		input until no further state transitions are possible. It always returns
		the longest match.

		If a character is read which has no transition defined, an
		`UnexpectedChar` exception is thrown.

		If the input is in the end of file state upon method invocation,
		`ruleset.eofFunction` is called with `this` Lexer as argument. If
		`ruleset` defines no `eofFunction` field, a `haxe.io.Eof` exception
		is thrown.

		If `ruleset` is null, the result is unspecified.
	**/
	public function token<T>(ruleset:Ruleset<T>):T {
		if (pos == input.length) {
			if (ruleset.eofFunction != null) return ruleset.eofFunction(this);
			else throw new haxe.io.Eof();
		}
		var state = ruleset.state;
		var lastMatch = null;
		var lastMatchPos = pos;
		var start = pos;

		#if expose_lexer_state
		stateCallback(state, pos, -1);
		#end

		while(true) {
			if (state.finalId > -1) {
				lastMatch = state;
				lastMatchPos = pos;
			}
			if (pos == input.length) {
				break;
			}
			var i = input.readByte(pos);
			++pos;
			state = state.trans.get(i);

			#if expose_lexer_state
			stateCallback(state, pos-1, i);
			#end

			if (state == null)
				break;
		}
		pos = lastMatchPos;
		current = input.readString(start, pos - start);
		if (lastMatch == null || lastMatch.finalId == -1)
			throw new UnexpectedChar(String.fromCharCode(input.readByte(pos)), curPos());
		return ruleset.functions[lastMatch.finalId](this);
	}

	#if expose_lexer_state
	/**

		@param	state	`null` if it's the last state visited
		@param	position	Position of the byte read
		@param	input	Transition input byte, `-1` if initial state
	**/
	dynamic public function stateCallback(state:State, position:Int, input:Int) {}
	#end

	/**
		Builds a `Ruleset` from the given `rules` `Array`.

		For each element of `rules`, its `rule` `String` is parsed into a
		`Pattern` using `LexEngine.parse`.

		If `rules` is null, the result is unspecified.
	**/
	static public function buildRuleset<Token>(rules:Array<{rule:String,func:Lexer->Token}>, name:String = "") {
		var cases = [];
		var functions = [];
		var eofFunction = null;
		for (rule in rules) {
			if (rule.rule == "") {
				eofFunction = rule.func;
			} else {
				cases.push(LexEngine.parse(rule.rule));
				functions.push(rule.func);
			}
		}
		return new Ruleset(new LexEngine(cases).firstState(), functions, eofFunction, name);
	}
}