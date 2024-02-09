abstract class Base<T = Bool> {
	var id:String;
	function new(id:String) {
		this.id = id;
	}

	public function getValue() {
		return id;
	}
}
