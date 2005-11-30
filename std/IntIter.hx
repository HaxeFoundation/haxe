class IntIter implements Iterator<Int> {

	var min : Int;
	var max : Int;

	public function new( min : Int, max : Int ) {
		this.min = min;
		this.max = max;
	}

	public function hasNext() {
		return min != max;
	}

	public function next() {
		if( min < max )
			return min++;
		return min--;
	}

}