package flash.media;

extern class AVSegmentedSource extends AVSource {
	@:flash.property var cookieHeader(never,set) : String;
	@:flash.property var masterUpdateInterval(never,set) : Int;
	@:flash.property var networkingParams(get,set) : AVNetworkingParams;
	@:flash.property var useRedirectedUrl(never,set) : Bool;
	function new() : Void;
	function addCustomHeader(headerName : String, args : flash.Vector<String>) : Void;
	function clearPauseAtPeriodEnd(periodIndex : Int) : AVResult;
	function getABRProfileCount(periodIndex : Int) : Int;
	function getABRProfileInfoAtIndex(periodIndex : Int, abrProfileIndex : Int) : AVABRProfileInfo;
	function getBackgroundPeriodInfo(periodIndex : Int) : AVPeriodInfo;
	function getBackgroundTimeline() : AVTimeline;
	function getCuePoint(periodIndex : Int, cuePointIndex : Int) : AVCuePoint;
	function getMediaPreferredStartTime() : Float;
	function getPerceivedBandwidth() : UInt;
	function getPeriodInfo(periodIndex : Int) : AVPeriodInfo;
	function getPeriodInfoWithTagsAcrossIndexes(periodIndex : Int, startPeriodIndex : Int) : AVPeriodInfo;
	function getSubscribedTag(periodIndex : Int, tagDataIndex : Int) : AVTagData;
	function getSubscribedTagForBackgroundManifest(periodIndex : Int, tagDataIndex : Int) : AVTagData;
	function getTimeline() : AVTimeline;
	function getTimelineSubscribedTag(tagDataIndex : Int) : AVTagData;
	function getTimelineSubscribedTagForBackgroundManifest(tagDataIndex : Int) : AVTagData;
	function getTrackCount(periodIndex : Int, payloadType : String) : Int;
	function getTrackInfo(periodIndex : Int, payloadType : String, trackIndex : Int) : AVTrackInfo;
	private function get_networkingParams() : AVNetworkingParams;
	function insertByLocalTime(periodIndex : Int, insertionTime : Float, handle : Int, userData : Int = 0, replaceDuration : Float = 0) : AVInsertionResult;
	function insertByVirtualTime(insertionTime : Float, handle : Int, userData : Int = 0, replaceDuration : Float = 0) :