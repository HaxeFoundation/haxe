# How to update flash player signatures in Brew

It's easiest to use a mac to do the update since there is a developer script provided by homebrew-cask that can semi-automate the thing.

Steps:
1. clone https://github.com/Homebrew/homebrew-cask
2. Run ./developer/bin/update_cask_family flash $NEW_VERSION_STRING

If homebrew-cask's CI succeed, the PR will be automatically merged by a bot, and our CI is saved.

The super annoying thing is that, homebrew-cask's CI will check Adobe's appcast for the version string, but the appcast is usually outdated until about a day after the new Flash Player release.

See https://github.com/Homebrew/homebrew-cask/pull/73950#issuecomment-563920561

----
Example PR: https://github.com/Homebrew/homebrew-cask/pull/73952