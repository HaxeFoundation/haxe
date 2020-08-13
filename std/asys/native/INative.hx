package asys.native;

import asys.native.filesystem.IFileSystem;

@:inheritDoc(asys.native.Native)
interface INative {
	@:inheritDoc(asys.native.Native.filesystem)
	var filesystem(get,never):IFileSystem;
}