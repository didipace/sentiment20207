# How to update flash player signatures in Brew

It's easiest to use a mac to do the update since there is a developer script provided by homebrew-cask that can semi-automate the thing.

Steps:
1. clone https://github.com/Homebrew/homebrew-cask
2. Run ./developer/bin/update_cask_family flash $NEW_VERSION_STRING

If homebrew-cask's CI succeed, the PR will be automatically merged by a bot, and our CI is 