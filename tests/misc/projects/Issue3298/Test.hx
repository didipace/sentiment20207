class Test {
    static function error(msg, code) {
        Sys.stderr().writeString(msg);
        Sys.exit(code);
    }

    static function main() {
        var proc = new sys