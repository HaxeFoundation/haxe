package refactor.typing;

interface ITyper {
	function resolveType(fileName:String, pos:Int):Promise<Null<TypeHintType>>;
}
