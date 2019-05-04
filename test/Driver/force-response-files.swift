// Ensure that -driver-force-response-files works.


// RUN: %swiftppc_driver -driver-force-response-files -typecheck %S/../Inputs/empty.swift -### 2>&1 | %FileCheck %s
// CHECK: @
// CHECK: .resp
