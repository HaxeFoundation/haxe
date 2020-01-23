package asyncio.system;

import asyncio.filesystem.FileOpenFlag;
import asyncio.filesystem.FilePath;
import sys.io.File;

enum ProcessIO {
	Ignore;
	Inherit;
	File(path:FilePath, flags:FileOpenFlag<Dynamic>);
	OpenFile(file:File);
}