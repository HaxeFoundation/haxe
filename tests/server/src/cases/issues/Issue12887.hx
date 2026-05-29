package cases.issues;

// An anonymous structure that mixes a field referencing the enclosing class's
// type parameter with a field whose own type is a (nested) anonymous structure
// was serialized incorrectly to hxb. The type-parameter field forces the anon
// to be written inline (it needs the class context to resolve a TPHType ref),
// setting `needs_local_context`. But the sibling nested-anon field goes through
// `write_anon_ref`, which unconditionally resets `needs_local_context` to false,
// clobbering the flag. The outer anon is then mis-deferred into the OBD chunk,
// and on re-read from cache it has no class context, so resolving the
// type-parameter reference indexes an empty `type_type_parameters` array:
//
//   Compiler failure while reading hxb chunk OBD of Container: index out of bounds
//
// Field iteration order matters: the type-parameter field (`a`) must be visited
// before the nested-anon field (`b`) for the reset to clobber a set flag, hence
// the alphabetical naming below.
class Issue12887 extends TestCase {
	function test(_) {
		vfs.putContent("Container.hx", "
			class Container<T> {
				public function new() {}
				public function make(value:T) {
					var x:{ a:T, b:{ x:Int } } = { a:value, b:{ x:0 } };
					return x;
				}
			}
		");
		vfs.putContent("Main.hx", "
			class Main {
				static function main() {
					var c = new Container<String>();
					c.make('hi');
				}
			}
		");

		var args = ["-main", "Main", "--interp", "--no-output"];

		// First build writes Container to the hxb cache.
		runHaxe(args);
		assertSuccess();

		// Second build reuses Container from cache, reading it back from hxb.
		// Without the fix this crashes while reading the OBD chunk.
		runHaxe(args);
		assertReuse("Container");
		assertSuccess();
	}
}
