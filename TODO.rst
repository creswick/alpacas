ALPACAS TODO
==================================================

- Minimal release features:

  - Root is a page that gives you the configuration in a textarea with
    a post button

- Bugs

  - If you call the reload action, force-recomp

    - It's hard to see how to pass the flag to the next invocation of itself

  - There's no way to update the ALPACAS library from within itself

  - You can get into a hosed state with a stale binary

  - Reload action doesn't always return a page

    - We're better off now that it's not returning a 500, so we can
      defer this for now

      The 500 status code was triggering `an infelicity in Chrome
      <http://code.google.com/p/chromium/issues/detail?id=66062>`_
      that made the browser refuse to send a request to the reload
      action.

  - `Snap <http://snapframework.com/>`_ doesn't let us exit the server
    without raising an `AsyncException
    <http://hackage.haskell.org/packages/archive/base/4.2.0.0/doc/html/Control-Exception.html#t%3AAsyncException>`_. Right
    now, we're using ``UserInterrupt`` to signal exit, but that makes
    the command-line interface gross. We need to submit a patch to
    ``snap-server`` that gives us a function to call that will exit
    the server.

    This will still be a bit tricky, because we'll have to decide what
    to do with the existing connections to the server.

  - Ideally, we'd hold on to the open connections when we restart the
    server (especially the server socket, and just let the connections
    queue until we're back up). This is a tricky one.

- Next features
  - Alpacase 'reset', to clear out the .config / .cache / etc. content

  - JS console

  - goto-line

  - find-file
    - File reading/writing via a crude textarea interface (finish
      bootstrapping)

  - Make a trivial web server 'kernel' that has very basic
    capabilities: edit config, reload, and proxies all "real" requests
    to a child process that runs the functional web server.  (This
    makes it so that you can see the console output while reloading,
    gives a safety net if you break things, etc..)

  - Packaging script that builds a repository

  - AJAX status about the reloading state

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
