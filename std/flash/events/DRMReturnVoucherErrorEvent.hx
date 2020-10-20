package flash.events;

extern class DRMReturnVoucherErrorEvent extends ErrorEvent {
	@:flash.property var licenseID(get,set) : String;
	@:flash.property var policyID(get,set) : String;
	@:flash.property var serverURL(get,set) : String;
	@:flash.property var subErrorID(get,set) : Int;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?inDetail : String, inErrorID : Int = 0, inSubErrorID : Int = 0, ?inServerURL : String, ?inLicenseID : String, ?inPolicyID : String) : Void;
	private function get_lice