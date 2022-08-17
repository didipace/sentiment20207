import utest.Assert;

import RootMod1;
import RootMod3.lowerCase3;
import RootMod3.UpperCase3;
import RootMod4.lowerCase as lowerCase4;
import RootMod4.UpperCase as UpperCase4;
import pack.Mod1;
import ModWithStaticAndClassStatic2;

class Main extends utest.Test {
	function testImportedModule() {
		Assert.equals("RootMod1.lowerCase", lowerCase());
		Assert.equals("RootMod1.UpperCase", UpperCase());
		Assert.equals("pack.Mod1.lowerCasePack", lowerCasePack());
		Assert.equals("pack.Mod1.UpperCasePack", UpperCasePack());
	}

	function testUnimportedRootModule() {
		Assert.equals("RootMod2.lowerCase", RootMod2.lowerCase());
		Assert.equals("RootMod2.UpperCase", RootMod2.UpperCase());
	}

	function testUnimportedRootModuleWithStd() {
		Assert.equals("RootMod2.lowerCase", std.RootMod2.lowerCase());
		Assert.equals("RootMod2.UpperCase", std.RootMod2.UpperCase());
	}

	function testUnimportedPackModule() {
		Assert.equals("pack.Mod2.lowerCasePack", pack.Mod2.lowerCasePack());
		Assert.equals("pack.Mod2.UpperCasePack", pack.Mod2.UpperCasePack());
	}

	function testUnimportedPackModuleWithStd() {
		Assert.equals("pack.Mod2.lowerCasePack", std.pack.Mod2.lowerCasePack());
		Assert.equals("pack.Mod2.UpperCasePack", std.pack.Mod2.UpperCasePack());
	}

	function testImportedFunction() {
		Assert.equals("RootMod3.lowerCase", lowerCase3());
		Assert.equals("RootMod3.UpperCase", UpperCase3());
	}

	function testImportedFunctionAliased() {
		Assert.equals("RootMod4.lowerCase", lowerCase4());
		Assert.equals("RootMod4.UpperCase", UpperCase4());
	}

	function testPrivate() {
		Assert.equals("ModWithPrivate.f", ModWithPrivate.f());
	}

	function testUnimportedModuleStaticBeforeMainClassStatic() {
		Assert.equals("