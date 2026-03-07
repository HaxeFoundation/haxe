enum Ordering {
	EQ;
	LT;
	GT;
  }

@:publicFields
enum abstract TriState(Ordering) from Ordering {
	var True = GT;
	var False = LT;
	var EvenFalser = LT;
	var Both = EQ;

	function icon():Int {
		return switch (this : TriState) {
			case True:1;
			case Both:3;
		}
	}
}

function main() {}