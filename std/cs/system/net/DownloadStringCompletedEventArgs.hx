package cs.system.net;

/** Provides data for the  event. */
@:native("System.Net.DownloadStringCompletedEventArgs")
extern class DownloadStringCompletedEventArgs extends cs.system.componentmodel.AsyncCompletedEventArgs {
	/**
	 * Gets the data that is downloaded by a  method.
	 * @return A  that contains the downloaded data.
	 */
	var Result(default, never):String;
}
