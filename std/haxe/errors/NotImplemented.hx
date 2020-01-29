package haxe.errors;

class NotImplemented extends Error {
	public function new(message:String = 'Not implemented', ?pos:PosInfos) {
		super(message, pos);
	}
}