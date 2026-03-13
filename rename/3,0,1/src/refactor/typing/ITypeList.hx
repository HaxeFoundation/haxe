package refactor.typing;

import refactor.discover.Type;

interface ITypeList {
	function getType(fullName:String):Null<Type>;
}
