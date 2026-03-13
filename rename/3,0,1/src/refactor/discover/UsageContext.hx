package refactor.discover;

import refactor.cache.IFileCache;

typedef UsageContext = {
	var fileReader:FileReaderFunc;
	var file:Null<File>;
	var fileName:String;
	var usageCollector:UsageCollector;
	var nameMap:NameMap;
	var fileList:FileList;
	var typeList:TypeList;
	var type:Null<Type>;
	var cache:Null<IFileCache>;
}
