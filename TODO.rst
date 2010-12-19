ALPACAS TODO
==================================================

- Minimal release features:

  - Root is a page that gives you the configuration in a textarea with
    a post button

- Bugs

  - If you call the reload action, force-recomp

    - It's hard to see how to pass the flag to the next invocation of itself

  - Namespace clutter

  - There's no way to update the Heart library from within itself

  - You can get into a hosed state with a stale binary

  - Reload action doesn't always return a page

    - We're better off now that it's not returning a 500, so we can
      defer this for now

      The 500 status code was triggering `an infelicity in Chrome
      <http://code.google.com/p/chromium/issues/detail?id=66062>`_
      that made the browser refuse to send a request to the reload
      action.

- Next features

  - File reading/writing via a crude textarea interface (finish
    bootstrapping)

  - Packaging script that builds a repository

- Minimum dependencies:

  - GHC 6.10 or higher

  - cabal

- Ideas

  - JS console at your fingertips to run stuff in your UI

  - git-backed config

  - shell support

  - OpenID/OAuth support!

  - github integration

    - Even with the OAuth!

  - UI compatibility

    - Look for that emacs emulator in Javascript

  - Needs a packaging system

    - can use Cabal packages for the most part

    - Needs a sandbox for packages

    - Installer that comes with its own GHC?

      - why not? Emacs does.

  - Can dl Python, PHP, etc.

  - Security plugins

  - Add gitit or other Wiki support, blogging, etc.

  - WYSIWYG HTML editor (FCK or similar)

  - Integration with other services like gmail, etc. (Your browser has
    all your cookies!)
