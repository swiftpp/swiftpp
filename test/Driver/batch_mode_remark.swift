// Ensure that driver does not issue a remark iff in batch mode. (The remark has been removed.)
//
// RUN: %swiftppc_driver -whole-module-optimization -enable-batch-mode   %S/../Inputs/empty.swift -### 2>&1 >/dev/null | %FileCheck %s
// RUN: %swiftppc_driver                                                 %S/../Inputs/empty.swift -### 2>&1 >/dev/null | %FileCheck -allow-empty %s
// RUN: %swiftppc_driver -enable-batch-mode                              %S/../Inputs/empty.swift -### 2>&1 >/dev/null | %FileCheck -allow-empty %s
// RUN: %swiftppc_driver -enable-batch-mode        -disable-batch-mode   %S/../Inputs/empty.swift -### 2>&1 >/dev/null | %FileCheck -allow-empty %s
//
// CHECK-NOT: remark: using batch mode
