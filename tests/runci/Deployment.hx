package runci;

import runci.Config.*;
import runci.System.*;
import sys.io.File;
import sys.FileSystem;
import haxe.*;
using StringTools;

class Deployment {
	static var S3_HXBUILDS_ADDR(default, null) = 's3://hxbuilds/builds/haxe';

	static var gitInfo(get, null):{repo:String, branch:String, commit:String, timestamp:Float, date:String};

	static function get_gitInfo() return if (gitInfo != null) gitInfo else gitInfo = {
		repo: commandResult("git", ["config", "--get", "remote.origin.url"]).stdout.trim(),
		branch: commandResult("git", ["rev-parse", "--abbrev-ref", "HEAD"]).stdout.trim(),
		commit: commandResult("git", ["rev-parse", "HEAD"]).stdout.trim(),
		timestamp: Std.parseFloat(commandResult("git", ["show", "-s", "--format=%ct", "HEAD"]).stdout),
		date: {
			var gitTime = commandResult("git", ["show", "-s", "--format=%ct", "HEAD"]).stdout;
			var tzd = {
				var z = Date.fromTime(0);
				z.getHours() * 60 * 60 * 1000 + z.getMinutes() * 60 * 1000;
			}
			// make time in the UTC time zone
			var time = Date.fromTime(Std.parseFloat(gitTime) * 1000 - tzd);
			DateTools.format(time, "%Y-%m-%dT%H:%M:%SZ");
		}
	}

	static function isDeployNightlies() {
		return
			Sys.getEnv("DEPLOY_NIGHTLIES") != null &&
			(gitInfo.branch == "development" || gitInfo.branch == "master" || gitInfo.branch == "deploy-test");
	}

	static function deployBintray():Void {
		if (
			Sys.getEnv("BINTRAY") != null &&
			Sys.getEnv("BINTRAY_USERNAME") != null &&
			Sys.getEnv("BINTRAY_API_KEY") != null
		) {
			// generate bintray config
			var tpl = new Template(File.getContent("extra/bintray.tpl.json"));
			var compatDate = ~/[^0-9]/g.replace(gitInfo.date, "");
			var json = tpl.execute({
				packageSubject: {
					var sub = Sys.getEnv("BINTRAY_SUBJECT");
					sub != null ? sub : Sys.getEnv("BINTRAY_USERNAME");
				},
				os: systemName.toLowerCase(),
				versionName: '${haxeVer}+${compatDate}.${gitInfo.commit.substr(0,7)}',
				versionDesc: "Automated CI build.",
				gitRepo: gitInfo.repo,
				gitBranch: gitInfo.branch,
				gitCommit: gitInfo.commit,
				gitDate: gitInfo.date,
			});
			var path = "extra/bintray.json";
			File.saveContent("extra/bintray.json", json);
			infoMsg("saved " + FileSystem.absolutePath(path) + " with content:");
			Sys.println(json);
		}
	}

	static function isDeployApiDocsRequired() {
		return
			Sys.getEnv("DEPLOY_API_DOCS") != null &&
			(
				gitInfo.branch == "development" ||
				switch(Sys.getEnv("TRAVIS_TAG")) { // TODO: there's no Travis anymore, we might want to change this for GH actions
					case null, _.trim() => "":
						false;
					case tag:
						true;
				}
			);
	}

	/**
		Deploy doc to api.haxe.org.
	*/

	static function deployApiDoc():Void {
		changeDirectory(repoDir);
		runCommand("make", ["xmldoc"]);
		File.saveContent("extra/doc/info.json", Json.stringify({
			"commit": gitInfo.commit,
			"branch": gitInfo.branch,
		}));
		switch (Sys.getEnv("GHP_REMOTE")) { // should be in the form of https://token@github.com/account/repo.git
			case null:
				infoMsg('Missing GHP_REMOTE, skip api doc deploy.');
			case remoteRepo:
				var localRepo = "extra/api.haxe.org";
				runCommand("git", ["clone", remoteRepo, localRepo]);
				runCommand("haxe", ["--cwd", localRepo, "--run", "ImportXml", FileSystem.abso