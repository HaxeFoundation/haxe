package refactor;

import refactor.VerboseLogger;
import refactor.discover.FileList;
import refactor.discover.FileReaderFunc;
import refactor.discover.NameMap;
import refactor.discover.TypeList;
import refactor.typing.ITyper;

typedef CacheAndTyperContext = {
	var nameMap:NameMap;
	var fileList:FileList;
	var typeList:TypeList;
	var verboseLog:VerboseLogger;
	var typer:Null<ITyper>;
	var fileReader:FileReaderFunc;
	var converter:ByteToCharConverterFunc;
}

typedef ByteToCharConverterFunc = (string:String, byteOffset:Int) -> Int;
