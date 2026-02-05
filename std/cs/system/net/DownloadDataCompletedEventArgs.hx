package cs.system.net;

/** Provides data for the  event. */
@:native("System.Net.DownloadDataCompletedEventArgs")
extern class DownloadDataCompletedEventArgs extends cs.system.componentmodel.AsyncCompletedEventArgs {
	/**
	 * Gets the data that is downloaded by a  method.
	 * @return A  array that contains the downloaded data.
	 */
	var Result(default, never):cs.NativeArray<cs.UInt8>;
}
