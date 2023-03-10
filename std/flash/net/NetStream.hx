
package flash.net;

extern class NetStream extends flash.events.EventDispatcher {
	@:flash.property var audioCodec(get,never) : UInt;
	@:flash.property @:require(flash10_1) var audioReliable(get,set) : Bool;
	@:flash.property @:require(flash10_1) var audioSampleAccess(get,set) : Bool;
	@:flash.property @:require(flash10_1) var backBufferLength(get,never) : Float;
	@:flash.property @:require(flash10_1) var backBufferTime(get,set) : Float;
	@:flash.property var bufferLength(get,never) : Float;
	@:flash.property var bufferTime(get,set) : Float;
	@:flash.property @:require(flash10_1) var bufferTimeMax(get,set) : Float;
	@:flash.property var bytesLoaded(get,never) : UInt;
	@:flash.property var bytesTotal(get,never) : UInt;
	@:flash.property var checkPolicyFile(get,set) : Bool;
	@:flash.property var client(get,set) : Dynamic;
	@:flash.property var currentFPS(get,never) : Float;
	@:flash.property @:require(flash10_1) var dataReliable(get,set) : Bool;
	@:flash.property var decodedFrames(get,never) : UInt;
	@:flash.property @:require(flash10) var farID(get,never) : String;
	@:flash.property @:require(flash10) var farNonce(get,never) : String;
	@:flash.property @:require(flash10_1) var inBufferSeek(get,set) : Bool;
	@:flash.property @:require(flash10) var info(get,never) : NetStreamInfo;
	@:flash.property var liveDelay(get,never) : Float;
	@:flash.property @:require(flash10) var maxPauseBufferTime(get,set) : Float;
	@:flash.property @:require(flash10_1) var multicastAvailabilitySendToAll(get,set) : Bool;
	@:flash.property @:require(flash10_1) var multicastAvailabilityUpdatePeriod(get,set) : Float;
	@:flash.property @:require(flash10_1) var multicastFetchPeriod(get,set) : Float;
	@:flash.property @:require(flash10_1) var multicastInfo(get,never) : NetStreamMulticastInfo;
	@:flash.property @:require(flash10_1) var multicastPushNeighborLimit(get,set) : Float;
	@:flash.property @:require(flash10_1) var multicastRelayMarginDuration(get,set) : Float;
	@:flash.property @:require(flash10_1) var multicastWindowDuration(get,set) : Float;
	@:flash.property @:require(flash10) var nearNonce(get,never) : String;
	@:flash.property var objectEncoding(get,never) : UInt;
	@:flash.property @:require(flash10) var peerStreams(get,never) : Array<Dynamic>;
	@:flash.property var soundTransform(get,set) : flash.media.SoundTransform;
	@:flash.property var time(get,never) : Float;
	@:flash.property @:require(flash11) var useHardwareDecoder(get,set) : Bool;
	@:flash.property @:require(flash11_3) var useJitterBuffer(get,set) : Bool;
	@:flash.property var videoCodec(get,never) : UInt;
	@:flash.property @:require(flash10_1) var videoReliable(get,set) : Bool;
	@:flash.property @:require(flash10_1) var videoSampleAccess(get,set) : Bool;
	@:flash.property @:require(flash11) var videoStreamSettings(get,set) : flash.media.VideoStreamSettings;
	function new(connection : NetConnection, ?peerID : String) : Void;
	@:require(flash10_1) function appendBytes(bytes : flash.utils.ByteArray) : Void;
	@:require(flash10_1) function appendBytesAction(netStreamAppendBytesAction : String) : Void;
	@:require(flash10_1) function attach(connection : NetConnection) : Void;
	function attachAudio(microphone : flash.media.Microphone) : Void;
	function attachCamera(theCamera : flash.media.Camera, snapshotMilliseconds : Int = -1) : Void;
	function close() : Void;
	@:require(flash11_2) function dispose() : Void;
	private function get_audioCodec() : UInt;
	private function get_audioReliable() : Bool;
	private function get_audioSampleAccess() : Bool;
	private function get_backBufferLength() : Float;
	private function get_backBufferTime() : Float;
	private function get_bufferLength() : Float;
	private function get_bufferTime() : Float;
	private function get_bufferTimeMax() : Float;
	private function get_bytesLoaded() : UInt;
	private function get_bytesTotal() : UInt;
	private function get_checkPolicyFile() : Bool;
	private function get_client() : Dynamic;
	private function get_currentFPS() : Float;
	private function get_dataReliable() : Bool;
	private function get_decodedFrames() : UInt;
	private function get_farID() : String;
	private function get_farNonce() : String;
	private function get_inBufferSeek() : Bool;
	private function get_info() : NetStreamInfo;
	private function get_liveDelay() : Float;
	private function get_maxPauseBufferTime() : Float;
	private function get_multicastAvailabilitySendToAll() : Bool;
	private function get_multicastAvailabilityUpdatePeriod() : Float;
	private function get_multicastFetchPeriod() : Float;
	private function get_multicastInfo() : NetStreamMulticastInfo;
	private function get_multicastPushNeighborLimit() : Float;
	private function get_multicastRelayMarginDuration() : Float;
	private function get_multicastWindowDuration() : Float;
	private function get_nearNonce() : String;
	private function get_objectEncoding() : UInt;
	private function get_peerStreams() : Array<Dynamic>;
	private function get_soundTransform() : flash.media.SoundTransform;
	private function get_time() : Float;
	private function get_useHardwareDecoder() : Bool;
	private function get_useJitterBuffer() : Bool;
	private function get_videoCodec() : UInt;
	private function get_videoReliable() : Bool;
	private function get_videoSampleAccess() : Bool;
	private function get_videoStreamSettings() : flash.media.VideoStreamSettings;
	@:require(flash10) function onPeerConnect(subscriber : NetStream) : Bool;
	function pause() : Void;
	function play(restArgs : haxe.extern.Rest<Dynamic>) : Void;
	@:require(flash10) function play2(param : NetStreamPlayOptions) : Void;
	function publish(?name : String, ?type : String) : Void;
	function receiveAudio(flag : Bool) : Void;
	function receiveVideo(flag : Bool) : Void;
	function receiveVideoFPS(FPS : Float) : Void;
	function resume() : Void;
	function seek(offset : Float) : Void;
	function send(handlerName : String, restArgs : haxe.extern.Rest<Dynamic>) : Void;
	private function set_audioReliable(value : Bool) : Bool;
	private function set_audioSampleAccess(value : Bool) : Bool;
	private function set_backBufferTime(value : Float) : Float;
	private function set_bufferTime(value : Float) : Float;
	private function set_bufferTimeMax(value : Float) : Float;
	private function set_checkPolicyFile(value : Bool) : Bool;
	private function set_client(value : Dynamic) : Dynamic;
	private function set_dataReliable(value : Bool) : Bool;
	private function set_inBufferSeek(value : Bool) : Bool;
	private function set_maxPauseBufferTime(value : Float) : Float;
	private function set_multicastAvailabilitySendToAll(value : Bool) : Bool;
	private function set_multicastAvailabilityUpdatePeriod(value : Float) : Float;
	private function set_multicastFetchPeriod(value : Float) : Float;
	private function set_multicastPushNeighborLimit(value : Float) : Float;
	private function set_multicastRelayMarginDuration(value : Float) : Float;
	private function set_multicastWindowDuration(value : Float) : Float;
	private function set_soundTransform(value : flash.media.SoundTransform) : flash.media.SoundTransform;
	private function set_useHardwareDecoder(value : Bool) : Bool;
	private function set_useJitterBuffer(value : Bool) : Bool;
	private function set_videoReliable(value : Bool) : Bool;
	private function set_videoSampleAccess(value : Bool) : Bool;
	private function set_videoStreamSettings(value : flash.media.VideoStreamSettings) : flash.media.VideoStreamSettings;
	@:require(flash10_1) function step(frames : Int) : Void;
	function togglePause() : Void;
	@:require(flash10) static final CONNECT_TO_FMS : String;
	@:require(flash10) static final DIRECT_CONNECTIONS : String;
	static function resetDRMVouchers() : Void;
}