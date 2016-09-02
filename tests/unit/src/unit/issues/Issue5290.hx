package unit.issues;

class Issue5290 extends unit.Test {
	function test() {
		var options = createFromPrototype({month: 0});
		eq(2016, options.year);
	}

	public static inline function createFromPrototype(
		options: { ?year: Int, ?month: Int })  {

		if(null == options.year)
			options.year = 2016;

		return options;
	}
}