// Ensure that driver does not issue a remark iff in batch mode. (The remark has been removed.)
//
// RUN: %ppswiftc_driver -whole-module-optimization -enable-batch-mode   %S/../Inputs/empty.swift -### 2>&1 >/dev/null | %FileCheck %s
// RUN: %ppswiftc_driver                                                 %S/../Inputs/empty.swift -### 2>&1 >/dev/null | %FileCheck -allow-empty %s
// RUN: %ppswiftc_driver -enable-batch-mode                              %S/../Inputs/empty.swift -### 2>&1 >/dev/null | %FileCheck -allow-empty %s
// RUN: %ppswiftc_driver -enable-batch-mode        -disable-batch-mode   %S/../Inputs/empty.swift -### 2>&1 >/dev/null | %FileCheck -allow-empty %s
//
// CHECK-NOT: remark: using batch mode
