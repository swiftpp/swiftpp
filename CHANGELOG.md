CHANGELOG
=========

<details>
<summary>Note: This is in reverse chronological order, so newer entries are added to the top.</summary>

| Contents                                |
| :-------------------------------------- |
| [Swift++ Hatchling](#swift++-hatchling) |

</details>

<a name="swift-hatchling" />

Swift++ Hatchling
-----------------

* Smalltalk-leaning changes to the syntax:

  The colon (':') character no longer delimits expressions because it's used to form Smalltalk keywords.
  Replaced the arrow ('->') with the return-type specifier ('::^'), and added the type specifier ('::').

  Removed prompting from lexer to convert single-quote string literals to double-quote.

  Added Smalltalk's double-quotes '\"' delimited comments.

  Removed emission of MarkDown-related tokens DocLineComment & DocBlockComment

* Builds complete without error:

  Renamed the driver to `swiftpp` and the compiler to `swiftppc`.

  Removed CommonMark references in the build system, and related dependencies in the code.

  In order to sever the compiler build system from the Swift ecosystem build system, development is
  proceeding in a source and build tree hierarchy which is separate from the Swift source and build
  tree hierarchy. This makes remaining dependencies Swift++ has on Swift's build system to cause
  problems with Swift++'s build, bringing the dependencies to my attention so they can be removed
  or mitigated. One such dependency is Swift's custom LLVM for Swift-specific support. I define
  environment variable **LLVM_BUILD_DIR** which specifies where the Swift-LLVM build directory is located.

  Omitting CommonMark and LLVM and most other non-core components. More will be removed as such
  components are identified.

  Hard-coded various settings in the build-scripts & cmake configuration files.

  Renamed lots of variations of Swift with Swift++.

**Add new entries to the top of this section, not here!**

