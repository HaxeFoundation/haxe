enum Tree<T> {
	Leaf(t:T);
	Node(l:Tree<T>, r:Tree<T>);
}

class Main {
	static function main() {

	}

	function testRedundance() {
		switch(true) {
			case false:
			case true:
			case false: // unused
		}

		switch(true) {
			case false | true:
			case true: // unused
			case false: // unused
		}

		switch(true) {
			case false
			| false: // unused
			case true:
		}

		switch(Leaf("foo")) {
			case Leaf(_)
				| Leaf("foo"): // unused
			case Node(l,r):
		}

		switch({s:"foo"}) {
			case { s : "foo" } :
			case { s : a } :
		}

		switch( { s:"foo", t:"bar" } ) {
			case { s : "foo" }:
			case { t : "bar" }:
			case { s : "foo", t:"bar" }: // unused
			case _:
		}

		switch ("foo") {
			case "foo":
			case x = "foo": // unused
		}

		switch ("foo") {
			case x = "foo":
			case "foo": // unused
		}

		switch [true] {
			case [true]:
			case [true]: // unused
			case [false]:
		}

		switch [true] {
			case [true]
			   | [true]: // unused
			case [false]:
		}
	}
}