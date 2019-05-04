// RUN: %swiftppc_driver_plain -v 2>&1 | %FileCheck %s
// RUN: %swiftppc_driver_plain -v -force-single-frontend-invocation 2>&1 | %FileCheck %s
// RUN: not %swiftppc_driver_plain -emit-executable 2>&1 | %FileCheck --check-prefix=CHECK-ERROR %s

// CHECK-NOT: error: no input files

// CHECK-ERROR: error: no input files
