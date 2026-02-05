package cs.system.net;

/** Provides data for the  event. */
@:native("System.Net.UploadFileCompletedEventArgs")
extern class UploadFileCompletedEventArgs extends cs.system.componentmodel.AsyncCompletedEventArgs {
	/**
	 * Gets the server reply to a data upload operation that is started by calling an 
	 * method.
	 * @return A  array that contains the server reply.
	 */
	var Result(default, never):cs.NativeArray<cs.UInt8>;
}
