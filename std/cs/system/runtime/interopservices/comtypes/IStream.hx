package cs.system.runtime.interopservices.comtypes;

/** Provides the managed definition of the  interface, with  functionality. */
@:native("System.Runtime.InteropServices.ComTypes.IStream")
extern interface IStream {
	/**
	 * Creates a new stream object with its own seek pointer that references the same
	 * bytes as the original stream.
	 * @param ppstm When this method returns, contains the new stream object. This
	 * parameter is passed uninitialized.
	 */
	function Clone(ppstm:cs.Ref<cs.system.runtime.interopservices.comtypes.IStream>):Void;
	/**
	 * Ensures that any changes made to a stream object that is open in transacted mode
	 * are reflected in the parent storage.
	 * @param grfCommitFlags A value that controls how the changes for the stream
	 * object are committed.
	 */
	function Commit(grfCommitFlags:Int):Void;
	/**
	 * Copies a specified number of bytes from the current seek pointer in the stream
	 * to the current seek pointer in another stream.
	 * @param pstm A reference to the destination stream.
	 * @param cb The number of bytes to copy from the source stream.
	 * @param pcbRead On successful return, contains the actual number of bytes read
	 * from the source.
	 * @param pcbWritten On successful return, contains the actual number of bytes
	 * written to the destination.
	 */
	function CopyTo(pstm:cs.system.runtime.interopservices.comtypes.IStream, cb:haxe.Int64, pcbRead:cs.system.IntPtr, pcbWritten:cs.system.IntPtr):Void;
	/**
	 * Restricts access to a specified range of bytes in the stream.
	 * @param libOffset The byte offset for the beginning of the range.
	 * @param cb The length of the range, in bytes, to restrict.
	 * @param dwLockType The requested restrictions on accessing the range.
	 */
	function LockRegion(libOffset:haxe.Int64, cb:haxe.Int64, dwLockType:Int):Void;
	/**
	 * Reads a specified number of bytes from the stream object into memory starting at
	 * the current seek pointer.
	 * @param pv When this method returns, contains the data read from the stream. This
	 * parameter is passed uninitialized.
	 * @param cb The number of bytes to read from the stream object.
	 * @param pcbRead A pointer to a  variable that receives the actual number of bytes
	 * read from the stream object.
	 */
	function Read(pv:cs.Ref<cs.NativeArray<cs.UInt8>>, cb:Int, pcbRead:cs.system.IntPtr):Void;
	/** Discards all changes that have been made to a transacted stream since the last  call. */
	function Revert():Void;
	/**
	 * Changes the seek pointer to a new location relative to the beginning of the
	 * stream, to the end of the stream, or to the current seek pointer.
	 * @param dlibMove The displacement to add to .
	 * @param dwOrigin The origin of the seek. The origin can be the beginning of the
	 * file, the current seek pointer, or the end of the file.
	 * @param plibNewPosition On successful return, contains the offset of the seek
	 * pointer from the beginning of the stream.
	 */
	function Seek(dlibMove:haxe.Int64, dwOrigin:Int, plibNewPosition:cs.system.IntPtr):Void;
	/**
	 * Changes the size of the stream object.
	 * @param libNewSize The new size of the stream as a number of bytes.
	 */
	function SetSize(libNewSize:haxe.Int64):Void;
	/**
	 * Retrieves the  structure for this stream.
	 * @param pstatstg When this method returns, contains a  structure that describes
	 * this stream object. This parameter is passed uninitialized.
	 * @param grfStatFlag Members in the  structure that this method does not return,
	 * thus saving some memory allocation operations.
	 */
	function Stat(pstatstg:cs.Ref<cs.system.runtime.interopservices.comtypes.STATSTG>, grfStatFlag:Int):Void;
	/**
	 * Removes the access restriction on a range of bytes previously restricted with
	 * the  method.
	 * @param libOffset The byte offset for the beginning of the range.
	 * @param cb The length, in bytes, of the range to restrict.
	 * @param dwLockType The access restrictions previously placed on the range.
	 */
	function UnlockRegion(libOffset:haxe.Int64, cb:haxe.Int64, dwLockType:Int):Void;
	/**
	 * Writes a specified number of bytes into the stream object starting at the
	 * current seek pointer.
	 * @param pv The buffer to write this stream to.
	 * @param cb The number of bytes to write to the stream.
	 * @param pcbWritten On successful return, contains the actual number of bytes
	 * written to the stream object. If the caller sets this pointer to , this method
	 * does not provide the actual number of bytes written.
	 */
	function Write(pv:cs.NativeArray<cs.UInt8>, cb:Int, pcbWritten:cs.system.IntPtr):Void;
}
