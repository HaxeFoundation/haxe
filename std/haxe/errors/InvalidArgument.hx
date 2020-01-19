package haxe.errors;

class InvalidArgument extends Error {
	public function new(message:String = 'Invalid argument') {
		super(message);
	}
}