package refactor.cache;

import refactor.discover.NameMap;
import refactor.discover.TypeList;

interface IFileCache {
	function save():Void;
	function clear():Void;
	function invalidateFile(name:String, nameMap:NameMap, typeList:TypeList):Void;
	function storeFile(file:refactor.discover.File):Void;
	function getFile(name:String, nameMap:NameMap):Null<refactor.discover.File>;
}
