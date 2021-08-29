package flash.events;

@:require(flash10_1) extern class DRMErrorEvent extends ErrorEvent {
	@:flash.property var contentData(get,set) : flash.net.drm.DRMContentData;
	@:flash.property var drmUpdateNeeded(get,never) : Bool;
	@:flash.property var subErrorID(get,never) : Int;
	@