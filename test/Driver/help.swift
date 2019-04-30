// Check that options printed with -help respect whether the driver is invoked
// as 'ppswift' or as 'ppswiftc'.

// RUN: %ppswiftc_driver -help | %FileCheck -check-prefix CHECK -check-prefix CHECK-SWIFTC %s
// RUN: %ppswiftc_driver -help | %FileCheck -check-prefix NEGATIVE -check-prefix NEGATIVE-SWIFTC %s

// RUN: %swift_driver -help | %FileCheck -check-prefix CHECK -check-prefix CHECK-SWIFT %s
// RUN: %swift_driver -help | %FileCheck -check-prefix NEGATIVE -check-prefix NEGATIVE-SWIFT %s

// Options that work with both 'ppswiftc' and 'ppswift':
// CHECK-DAG: -swift-version

// ppswiftc-only options:
// CHECK-SWIFTC-DAG: -typecheck
// NEGATIVE-SWIFT-NOT: -typecheck

// There are currently no interpreter-only options.

// Frontend options should not show up here.
// NEGATIVE-NOT: -merge-modules

// Options marked "help-hidden" should not show up here.
// NEGATIVE-NOT: -parse-stdlib
