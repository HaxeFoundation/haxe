package flash.media;

extern class AVSegmentedSource extends AVSource {
	function new() : Void;
	function getABRProfileCount(periodIndex : Int) : Int;
	function getABRProfileInfoAtIndex(periodIndex : Int, abrProfileIndex : Int) : AVABRProfileInfo;
	function getCuePoint(periodIndex : Int, cuePointIndex : Int) : AVCuePoint;
	function getPeriodInfo(periodIndex : Int) : AVPeriodInfo;
	function getTimeline() : AVTimeline;
	function getTrackCount(periodIndex : Int, payloadType : String) : Int;
	function getTrackInfo(periodIndex : Int, payloadType : String, trackIndex : Int) : AVTrackInfo;
	function insertByLocalTime(periodIndex : Int, insertionTime : Float, handle : Int, userData : Int = 0, replaceDuration : Float = 0) : AVInsertionResult;
	function insertByVirtualTime(insertionTime : Float, handle : Int, userData : Int = 0, replaceDuration : Float = 0) : AVInsertionResult;
	function load(url : String, ?containerType : String, userData : Int = 0) : AVResult;
	function loadManifest(uri : String, userData : Int = 0, ?containerType : String) : AVResult;
	function releaseManifest(handle : Int) : AVResult;
	function removeByLocalTime(periodIndex : Int, timeStart : Float, timeEnd : Float) : AVResult;
	function removeByVirtualTime(virtualTimeStart : Float, virtualTimeEnd : Float) : AVResult;
	function selectTrack(periodIndex : Int, payloadType : String, trackIndex : Int) : AVResult;
	function setABRParameters(params : AVABRParameters) : AVResult;
	function setBandwidth(bitsPerSecond : Int) : AVResult;
	function setHoldAt(time : Float) : AVResult;
	static var AUDIO : String;
	static var DATA : String;
	static var HLS : String;
	static var VIDEO : String;
}
