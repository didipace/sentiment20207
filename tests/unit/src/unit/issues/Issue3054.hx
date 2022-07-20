package unit.issues;

private enum MyEnum {
	A;
	B;
}

private enum MyOtherEnum {
	A(e:Null<MyEnum>);
	B;
}

class Issue3054 extends Test {
	function test() {
		var myVal