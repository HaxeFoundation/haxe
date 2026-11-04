class InterfaceFields implements IFace {
	static function main() {}

	public var wrongAccess(never,null):String;
	public function wrongKind():String {
		return null;
	}
}

interface IFace {
	var missing:String;
	var wrongAccess(default,null):String;
	var wrongKind:String;
}