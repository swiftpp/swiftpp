// RUN: %target-ppswiftc_driver -### %s | %FileCheck %s

// REQUIRES: autolink-extract

// CHECK: swift-autolink-extract {{.+}}.o -o {{.+}}.autolink
