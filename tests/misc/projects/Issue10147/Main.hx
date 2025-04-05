typedef HasAString = {
	text:String
}
typedef HasANullString = {
	text:Null<String>
}

@:nullSafety(StrictThreaded)
class Main {
	static function main() {
		final has:HasAString = {text: null};

		final tmp = {text: null};
		final typed:HasAString = tmp;

		final tmp = {text: null};
		final typed:{text:String} = tmp;

		final tmp = {text: null};
		final arr:Array<String> = [tmp.text];

		// should pass
		final tmp = {text: null};
		final typed:{text:Null<String>} = tmp;
		final typed2:HasANullString = tmp;
		final typed3:HasANullString = typed;
	}
}
